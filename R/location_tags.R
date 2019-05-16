#' Get top tags for a location
#'
#' Takes user defined location and returns the top tags related to the location.
#' Uses the flickr.places.tagsForPlace API method from the Flickr API. See
#' \url{https://www.flickr.com/services/api/flickr.places.tagsForPlace.html} for
#' more information on the API method.
#'
#' @param woe_id numeric. a "Where on Earth" location tag.
#'
#' @return character. List of the top 100 tags associated with the woe_id.
#' @export
#'
#' @examples
#' \dontrun{
#' location_tags(woe_id = 35356)
#' }
location_tags <- function(woe_id) {

  # this checks for the presence of a key, if no key it prompts the user to create one,
  # it then checks the validity of the key
  api_key <- create_and_check_key()

  get_tags <- paste("https://api.flickr.com/services/rest/?method=flickr.places.tagsForPlace&api_key=", api_key, "&woe_id=", woe_id, sep = "")

  # this new one works here
  tag_xml <- search_url(base_url = get_tags)

  if (!is.null(tag_xml)) {
    tag_atts <- xml2::xml_find_all(tag_xml, "//tag", ns = xml2::xml_ns(tag_xml))
    tags <- unlist(xml2::as_list(tag_atts))
  }
}
