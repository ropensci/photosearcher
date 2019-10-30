#' @noRd

# build search url
get_url <- function(mindate_taken,
                    maxdate_taken,
                    mindate_uploaded = NULL,
                    maxdate_uploaded = NULL,
                    user_id = NULL,
                    api_key,
                    page,
                    text = NULL,
                    tags = NULL,
                    tag_mode = NULL,
                    bbox = NULL,
                    woe_id = NULL,
                    has_geo = TRUE) {

  if (is.null(mindate_uploaded)){

    mindate_uploaded = mindate_taken

  }

  if (is.null(maxdate_uploaded)){

    maxdate_uploaded = maxdate_taken

  }


  # remove whitespace to allow proper encoding
  text <- gsub(" ", "+", trimws(text))
  tags <- gsub(" ", "+", trimws(tags))
  tags <- paste(tags, collapse = ",")

  mindate_taken <- gsub(" ", "+", trimws(mindate_taken))
  maxdate_taken <- gsub(" ", "+", trimws(maxdate_taken))

  base_url <- paste("https://api.flickr.com/services/rest/",
                    "?method=flickr.photos.search&api_key=", api_key,
                    "&text=", text,
                    "&tags=", tags,
                    "&tag_mode=", tag_mode,
                    "&min_taken_date=", as.character(mindate_taken),
                    "&max_taken_date=", as.character(maxdate_taken),
                    ifelse(!(is.null(mindate_uploaded)), paste0(
                      "&min_upload_date=", mindate_uploaded), ""),
                    ifelse(!(is.null(maxdate_uploaded)), paste0(
                      "&max_upload_date=", maxdate_uploaded), ""),
                    ifelse(!(is.null(user_id)), paste0(
                      "&user_id=", user_id), ""),
                    ifelse(!(is.null(bbox)), paste0("&bbox=", bbox), ""),
                    ifelse(!(is.null(woe_id)), paste0("&woe_id=", woe_id), ""),
                    ifelse(has_geo, paste0("&has_geo=", has_geo), ""),
                    "&extras=", "description,date_taken,geo,tags,license,",
                    "url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,",
                    "url_l,url_o,count_views,count_comments,count_faves,",
                    "date_upload,last_update",
                    "&page=", page,
                    "&sort=date-taken-asc",
                    "&format=", "rest",
                    sep = ""
  )
  return(base_url)
}


# search url
search_url <- function(base_url) {

  # get total number of results
  r <- httr::GET(base_url)

  # put first error catch here
  count_stat <- 0

  while (r$status_code != 200 & count_stat < 3) {
    Sys.sleep(0.5)
    r <- httr::GET(base_url)
    count_stat <- count_stat + 1
  }

  if (r$status_code != 200) {
    warning("Status code:", r$status, " for ", base_url,
            " - message: ", httr::content(r, "text"))
  }

  error <- tryCatch({
    photo_xml <- xml2::read_xml(r)
    error <- "success"
  }, error = function(err) {
    warning(base_url, " skipped beacuse: ", err)
    error <- "error"
    photo_xml <- NULL
  })

  return(photo_xml)
}

#error warnings
find_errors <- function(error_xml = NULL){

  if (xml2::xml_attrs(error_xml) == "fail"){

  warn_data <- data.frame(xml2::xml_attrs(xml2::xml_children(error_xml)))
  warn <- as.character(unlist(warn_data))

  stop(paste(warn[2]))

  }
}


# for the todo bullet points
ui_todo <- function(x, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = .envir)
  x <- gsub("\n", paste0("\n", "  "), x)
  x <- paste0(crayon::red(clisymbols::symbol$bullet), " ", x)
  lines <- paste0(x, "\n")
  cat(lines, sep = "")
}

# for the info bullet points
ui_info <- function(x, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = .envir)
  x <- paste0(crayon::yellow(clisymbols::symbol$info), " ", x)
  lines <- paste0(x, "\n")
  cat(lines, sep = "")
}

# this checks for the presence of a key, if no key it prompts the user to create
# one, it then checks the validity of the key
create_and_check_key <- function() {
  if (!file.exists("photosearcher_key.sysdata")) {
    ui_todo(
      "Create a Flickr API key at https://www.flickr.com/services/apps/create/")

    utils::browseURL("https://www.flickr.com/services/apps/create/")

    ui_todo("Enter your Flickr API key:")

    utils::write.table(readline(),
                       file = "photosearcher_key.sysdata",
                       col.names = FALSE,
                       row.names = FALSE)
  }

  api_key <- utils::read.table("photosearcher_key.sysdata", stringsAsFactors = FALSE)

  base_url <- paste("https://api.flickr.com/services/rest/",
                    "?method=flickr.photos.search&api_key=",
                    api_key,
                    sep = "")

  photo_xml <- search_url(base_url = base_url)
  pages_data <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
  warn <- as.character(unlist(pages_data))

  if ((warn[2]) == ("Invalid API Key (Key has invalid format)")) {
    stop("Invalid API Key: correct this in photosearcher_key.sysdata")
  }

  return(api_key)
}

# check that flickr locaiton services are working and woe_id is valid
check_location <- function(api_key = NULL) {

    known_location <- paste("https://api.flickr.com/services/rest/",
                            "?method=flickr.photos.search&api_key=",
                            api_key,
                            "&woe_id=35356",
                            sep = "")

    r <- httr::GET(known_location)
    photo_xml <- xml2::read_xml(r)
    known_warn <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))

    if ((known_warn[2, 1]) == ("Not a valid place type")) {
      stop("Flickr location services are down")
    }
  }

create_bbox <- function(sf_layer = NULL){

  # find crs
  layer_epsg <- unlist(sf::st_crs(sf_layer)[1])


  # transform if needed
  if ((is.na(layer_epsg)) | (layer_epsg != 4326)) {
    sf_layer <- sf::st_transform(
      sf_layer, crs = "+proj=longlat +datum=WGS84 +no_defs")
  }

  # generate bbox
  bbox <- sf::st_bbox(sf_layer)

  xmin <- bbox[1]
  ymin <- bbox[2]
  xmax <- bbox[3]
  ymax <- bbox[4]

  # bbox for url search
  bbox <- as.character(paste(
    xmin, ",", ymin, ",", xmax, ",", ymax, sep = ""))
}

#parse dataframe for output
parse_pic <- function(pics = NULL){

  pics <- data.frame(lapply(pics, as.character), stringsAsFactors=FALSE)

  pics$datetaken <- as.POSIXct(pics$datetaken)

  cols.num <- c("id",
                "server",
                "farm",
                "latitude",
                "longitude",
                "woeid",
                "height_sq",
                "width_sq",
                "height_t",
                "width_t",
                "height_s",
                "width_s",
                "height_q",
                "width_q",
                "height_m",
                "width_m",
                "height_n",
                "width_n",
                "height_z",
                "width_z",
                "height_c",
                "width_c",
                "height_l",
                "width_l" ,
                "height_o",
                "width_o")

  pics[cols.num] <- sapply(pics[cols.num],as.numeric)

  cols.num <- c("license",
                "datetakengranularity",
                "datetakenunknown",
                "count_views",
                "count_faves",
                "count_comments",
                "accuracy",
                "context")

  pics[cols.num] <- sapply(pics[cols.num],as.integer)

  cols.num <- c("ispublic",
                "isfriend",
                "isfamily",
                "geo_is_family",
                "geo_is_friend",
                "geo_is_contact",
                "geo_is_public")

  pics[cols.num] <- sapply(pics[cols.num],as.logical)

  #format all dates to dates
  pics$dateupload <- as.POSIXct(as.numeric(pics$dateupload),
                                tz = "GMT", origin="1970-01-01")

  pics$lastupdate <- as.POSIXct(as.numeric(pics$lastupdate),
                               tz = "GMT", origin="1970-01-01")

  #provide clearer license information
  license_names <- c("All Rights Reserved",
                     "Attribution-NonCommercial-ShareAlike License",
                     "Attribution-NonCommercial License",
                     "Attribution-NonCommercial-NoDerivs License",
                     "Attribution License",
                     "Attribution-ShareAlike License",
                     "Attribution-NoDerivs License",
                     "No known copyright restrictions",
                     "United States Government Work",
                     "Public Domain Dedication (CC0)",
                     "Public Domain Mark"
  )

  license_urls <- c("NA",
                    "https://creativecommons.org/licenses/by-nc-sa/2.0/",
                    "https://creativecommons.org/licenses/by-nc/2.0/",
                    "https://creativecommons.org/licenses/by-nc-nd/2.0/",
                    "https://creativecommons.org/licenses/by/2.0/",
                    "https://creativecommons.org/licenses/by-sa/2.0/",
                    "https://creativecommons.org/licenses/by-nd/2.0/",
                    "https://www.flickr.com/commons/usage/",
                    "http://www.usa.gov/copyright.shtml",
                    "https://creativecommons.org/publicdomain/zero/1.0/",
                    "https://creativecommons.org/publicdomain/mark/1.0/"
  )

  license_info <- data.frame(license = 0:10,
                             license_name = license_names,
                             license_url = license_urls,
                             stringsAsFactors = FALSE)

  pics <- merge(pics, license_info, by = "license")

  return(pics)

  }

