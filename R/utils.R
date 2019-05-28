#' @noRd

# search url
search_url <- function(base_url) {

  # get total number of results
  r <- httr::GET(base_url, encoding = "ISO-8859")

  # put first error catch here
  count_stat <- 0

  while (r$status_code != 200 & count_stat < 3) {
    Sys.sleep(0.5)
    r <- httr::GET(base_url, encoding = "ISO-8859")
    count_stat <- count_stat + 1
  }

  if (r$status_code != 200) {
    warning("Status code:", r$status, " for ", base_url, " - message: ", httr::content(r, "text", encoding = "ISO-8859"))
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

# build search url
get_url <- function(mindate,
                    maxdate,
                    api_key,
                    page,
                    text = NULL,
                    tags = NULL,
                    bbox = NULL,
                    woe_id = NULL,
                    has_geo = TRUE) {

  if(!is.null(bbox) & !is.null(woe_id)){
    stop("Specify location as either woe_id or bbox, not both.")
  }

    base_url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", api_key,
    "&text=", text,
    "&tags=", tags,
    "&min_taken_date=", as.character(mindate),
    "&max_taken_date=", as.character(maxdate),
    ifelse(!(is.null(bbox)), paste0("&bbox=", bbox), ""),
    ifelse(!(is.null(woe_id)), paste0("&woe_id=", woe_id), ""),
    ifelse(has_geo, paste0("&has_geo=", has_geo), ""),
    "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o,count_views,count_comments,count_faves",
    "&page=", page,
    "&format=", "rest",
    sep = ""
  )
  return(base_url)
}


#check for valid bbox
check_bbox <- function(bb, key){

  base_url = paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", key,"&bbox=", bb, sep = "")

  photo_xml <- search_url(base_url = base_url)
  pages_data <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
  warn <- as.character(unlist(pages_data))

  if ((warn[2]) == ("Not a valid bounding box")){
    stop("Not a valid bounding box")
  }

}

# for the todo bullet points
ui_todo <- function (x, .envir = parent.frame())
{
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

# this checks for the presence of a key, if no key it prompts the user to create one, it then checks the validity of the key
create_and_check_key <- function() {
  if(!file.exists("api_key.txt")) {
    ui_todo("Create a Flickr API key at https://www.flickr.com/services/apps/create/")
    utils::browseURL("https://www.flickr.com/services/apps/create/")
    ui_todo("Enter your Flickr API key:")
    write.table(readline(), file = "api_key.txt", col.names = FALSE, row.names = FALSE)
  }

  api_key <- utils::read.table("api_key.txt", stringsAsFactors = FALSE)

  base_url = paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", api_key, sep = "")

  photo_xml <- search_url(base_url = base_url)
  pages_data <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
  warn <- as.character(unlist(pages_data))

  if ((warn[2]) == ("Invalid API Key (Key has invalid format)")){
    stop("Invalid API Key: correct this in api_key.txt")
  }

  return(api_key)
}


