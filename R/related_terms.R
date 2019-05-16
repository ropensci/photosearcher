#' Get related terms
#'
#' Takes a term and returns the top tags related to that term.
#'
#' Uses the flickr.tags.getRelated API method from the Flickr API. See
#' \url{https://www.flickr.com/services/api/flickr.tags.getRelated.html} for
#' more information on the API method.
#'
#' @param term character. Term to search.
#'
#' @return character. Tags most associated with input term.
#' @export
#'
#' @examples
#' \dontrun{
#' related_terms(term = "car")
#'
#' related_terms(term = "monkey")
#'
#' related_terms(term = "river")
#' }
related_terms <- function(term) {

  # this checks for the presence of a key, if no key it prompts the user to create one,
  # it then checks the validity of the key
  api_key <- create_and_check_key()

  term <- gsub(" ", "+", trimws(term))

  baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.tags.getRelated&api_key=", api_key, "&tag=", term, sep = "")

  tag_xml <- search_url(base_url = baseURL)

  if (!is.null(tag_xml)) {
    tag_atts <- xml2::xml_find_all(tag_xml, "//tag", ns = xml2::xml_ns(tag_xml))
    tags <- unlist(xml2::as_list(tag_atts))
  }

  return(tags)
}
