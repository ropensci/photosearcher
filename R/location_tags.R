#' location_tags
#'
#' @param woe_id Numeric, a "Where on Earth" location tag.
#'
#' @return List of the top 100 tags associated with the woe_id.
#' @export
#'
#' @examples
#' \dontrun{
#' location_tags(woe_id = 35356)
#' }
location_tags <- function(woe_id = NULL) {
  if (is.null(woe_id) == TRUE) {
    stop("provide woe_id")
  }

  api_key <- as.character(get_key())

  woe_id <- "35356"
  get_tags <- paste("https://api.flickr.com/services/rest/?method=flickr.places.tagsForPlace&api_key=", api_key, "&woe_id=", woe_id, sep = "")

  # this new one works here
  tag_xml <- search_url(base_url = get_tags)

  if (!is.null(tag_xml)) {
    tag_atts <- xml2::xml_find_all(tag_xml, "//tag", ns = xml2::xml_ns(tag_xml))
    tags <- data.frame(unlist(xml2::as_list(tag_atts)))
  }
}
