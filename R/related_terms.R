#' Title
#'
#' @param term String, term to search.
#'
#' @return List of terms most associated with input term.
#' @export
#'
#' @examples
#' related_terms(term = "car")
#'
#' related_terms(term = "monkey")
#'
#' related_terms(term = "river")
related_terms <-
  function(term = NULL) {
    if (is.null(term) == TRUE) {
      stop("provide a term")
    }

    term <- gsub(" ", "+", trimws(term))

    baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.tags.getRelated&api_key=", api_key, "&tag=", term, sep = "")

    tag_xml <- search_url(base_url = baseURL)

    if (!is.null(tag_xml)) {
      tag_atts <- xml2::xml_find_all(tag_xml, "//tag", ns = xml2::xml_ns(tag_xml))
      tags <- data.frame(unlist(xml2::as_list(tag_atts)))
    }

    return(tags)
  }
