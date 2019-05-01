#' @noRd

# search url
search_url <- function(base_url = base_url) {

  # get total number of results
  r <- httr::GET(paste(base_url))

  # put first error catch here
  count_stat <- 0

  while (r$status_code != 200 & count_stat < 3) {
    Sys.sleep(0.5)
    r <- httr::GET(paste(base_url))
    count_stat <- count_stat + 1
  }

  if (r$status_code != 200) {
    warning("Status code:", r$status, " for ", base_url, " - message: ", httr::content(r, "text"))
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
get_url <- function(mindate = "2019-01-01",
                    maxdate = "2019-01-01",
                    api_key,
                    page,
                    text = NULL,
                    tags = NULL,
                    bbox = NULL,
                    has_geo = TRUE) {
  base_url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", api_key,
    "&text=", text,
    "&tags=", tags,
    "&min_taken_date=", as.character(mindate),
    "&max_taken_date=", as.character(maxdate),
    ifelse(!(is.null(bbox)), paste0("&bbox=", bbox), ""),
    ifelse(has_geo, paste0("&has_geo=", has_geo), ""),
    "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o,count_views,count_comments,count_faves",
    "&page=", page,
    "&format=", "rest",
    sep = ""
  )
  return(base_url)
}

#check for valid key
check_key <- function(key = api_key){

  base_url = paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", key, sep = "")

  photo_xml <- search_url(base_url = base_url)
  pages_data <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
  warn <- as.character(unlist(pages_data))

  if ((warn[2]) == ("Invalid API Key (Key has invalid format)")){
    stop("Invalid API Key (Key has invalid format)")
  }

}

#check for valid bbox
check_bbox <- function(bb = bbox, key = api_key){

  base_url = paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", key,"&bbox=", bb, sep = "")

  photo_xml <- search_url(base_url = base_url)
  pages_data <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
  warn <- as.character(unlist(pages_data))

  if ((warn[2]) == ("Not a valid bounding box")){
    stop("Not a valid bounding box")
  }

}
