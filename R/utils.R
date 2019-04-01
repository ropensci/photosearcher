#' @noRd

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
    warning("Status code:", r$status, " for ", base_url, " - message: ", content(r, "text"))
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
