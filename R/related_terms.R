#' related_terms
#'
#' Takes user defined term and returns the top tags related to the term.
#' Uses the flickr.tags.getRelated API method from the Flickr API. See
#' \url{https://www.flickr.com/services/api/flickr.tags.getRelated.html} for
#' more information on the API method.
#'
#' Note: if this is the first function of the package and you do not enter you
#' API key in the arguement api_key you use you will be prompted to enter your
#' API key or save it using the save_key function. API keys are avialable from
#' \url{https://www.flickr.com/services/apps/create/apply}. Using the save_key
#' function will save your key as a .Rda file which can then be called to when
#' using any other function.
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

    # this checks for the presence of a key, if no key it prompts the user to create one,
    # it then checks the validity of the key
    api_key <- create_and_check_key()

    term <- gsub(" ", "+", trimws(term))

    baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.tags.getRelated&api_key=", api_key, "&tag=", term, sep = "")

    tag_xml <- search_url(base_url = baseURL)

    if (!is.null(tag_xml)) {
      tag_atts <- xml2::xml_find_all(tag_xml, "//tag", ns = xml2::xml_ns(tag_xml))
      tags <- data.frame(unlist(xml2::as_list(tag_atts)))
    }

    return(tags)
  }
