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
#' @param api_key String, if you have used the save_key function the api_key
#'   argument is automatically filled. If not api_key can be used optionally to
#'   supplying your API key if you do not wish for it to be saved in the
#'   environment or as a .Rda
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
  function(term = NULL, api_key = NULL) {
    if (is.null(term) == TRUE) {
      stop("provide a term")
    }

    # get or save the api_key
    if (is.null(api_key)) {
      stop("Enter API key or save using the save_key function")
    }

    #check for vailid key
    check_key(key = api_key)

    term <- gsub(" ", "+", trimws(term))

    baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.tags.getRelated&api_key=", api_key, "&tag=", term, sep = "")

    tag_xml <- search_url(base_url = baseURL)

    if (!is.null(tag_xml)) {
      tag_atts <- xml2::xml_find_all(tag_xml, "//tag", ns = xml2::xml_ns(tag_xml))
      tags <- data.frame(unlist(xml2::as_list(tag_atts)))
    }

    return(tags)
  }
