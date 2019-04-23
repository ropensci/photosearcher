#' related_terms
#'
#' Takes user defined term and returns the top tags related to the term.
#' Uses the flickr.tags.getRelated API method from the Flickr API. See
#' \url{https://www.flickr.com/services/api/flickr.tags.getRelated.html} for
#' more information on the API method.
#'
#' Note: if this is the first function of the package you use you will be
#' prompted to enter your API key. API keys are avialable from
#' \url{https://www.flickr.com/services/apps/create/apply}. The API key will
#' then be saved as a .Rda file and be called to when using any other function.
#'
#' @param term String, term to search.
#'
#' @return List of terms most associated with input term.
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
related_terms <-
  function(term = NULL) {
    if (is.null(term) == TRUE) {
      stop("provide a term")
    }

    api_key <- as.character(get_key())

    term <- gsub(" ", "+", trimws(term))

    baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.tags.getRelated&api_key=", api_key, "&tag=", term, sep = "")

    tag_xml <- search_url(base_url = baseURL)

    if (!is.null(tag_xml)) {
      tag_atts <- xml2::xml_find_all(tag_xml, "//tag", ns = xml2::xml_ns(tag_xml))
      tags <- data.frame(unlist(xml2::as_list(tag_atts)))
    }

    return(tags)
  }
