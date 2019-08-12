#' Get related tags
#'
#' Takes a tag and returns the top tags related to that tag.
#'
#' Uses the flickr.tags.getRelated API method from the Flickr API. See
#' \url{https://www.flickr.com/services/api/flickr.tags.getRelated.html} for
#' more information on the API method.
#'
#' @param tag character. tag to search.
#'
#' @return character. Tags most associated with input tag.
#' @export
#'
#' @examples
#' \dontrun{
#' related_tags(tag = "car")
#'
#' related_tags(tag = "monkey")
#'
#' related_tags(tag = "river")
#' }
related_tags <- function(tag) {

  # this checks for the presence of a key, if no key it prompts the user to
  # create one, it then checks the validity of the key
  api_key <- create_and_check_key()

  out <- NULL

  tag <- gsub(" ", "+", trimws(tag))

  baseURL <- paste("https://api.flickr.com/services/rest/",
                   "?method=flickr.tags.getRelated&api_key=",
                   api_key,
                   "&tag=",
                   tag,
                   sep = "")

  tag_xml <- search_url(base_url = baseURL)

  find_errors(error_xml = tag_xml)

  if (!is.null(tag_xml)) {
    tag_atts <- xml2::xml_find_all(tag_xml, "//tag", ns = xml2::xml_ns(tag_xml))
    out <- unlist(xml2::as_list(tag_atts))
  }

  return(out)
}
