#' location_tags
#'
#' Takes user defined location and returns the top tags related to the location.
#' Uses the flickr.places.tagsForPlace API method from the Flickr API. See
#' \url{https://www.flickr.com/services/api/flickr.places.tagsForPlace.html} for
#' more information on the API method.
#'
#' 'Note: if this is the first function of the package and you do not enter you
#' API key in the arguement api_key you use you will be prompted to enter your
#' API key. API keys are avialable from
#' \url{https://www.flickr.com/services/apps/create/apply}. The API key will then
#' be saved as a .Rda file and be called to when using any other function.
#'
#' @param woe_id Numeric, a "Where on Earth" location tag.
#' @param api_key String, optional method for supplying your API key if you do not
#'  wish for it to be saved in the environment or as a .Rda
#'
#' @return List of the top 100 tags associated with the woe_id.
#' @export
#'
#' @examples
#' \dontrun{
#' location_tags(woe_id = 35356)
#' }
location_tags <- function(woe_id = NULL, api_key = NULL) {
  if (is.null(woe_id) == TRUE) {
    stop("provide woe_id")
  }

  # get or save the api_key
  if (!is.null(api_key)){
    api_key <- api_key
  } else {
    api_key <- as.character(get_key())
  }

  get_tags <- paste("https://api.flickr.com/services/rest/?method=flickr.places.tagsForPlace&api_key=", api_key, "&woe_id=", woe_id, sep = "")

  # this new one works here
  tag_xml <- search_url(base_url = get_tags)

  if (!is.null(tag_xml)) {
    tag_atts <- xml2::xml_find_all(tag_xml, "//tag", ns = xml2::xml_ns(tag_xml))
    tags <- data.frame(unlist(xml2::as_list(tag_atts)))
  }
}
