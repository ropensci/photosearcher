#' Find correct location informatio for a given place
#'
#' Takes user defined location and returns location data for the search. Uses
#' the flickr.places.find API method from the Flickr API. See
#' \url{https://www.flickr.com/services/api/flickr.places.find.html} for more
#' information on the API method.
#'
#' Note: if this is the first function of the package and you do not enter you
#' API key in the arguement api_key you use you will be prompted to enter your
#' API key or save it using the save_key function. API keys are avialable from
#' \url{https://www.flickr.com/services/apps/create/apply}. Using the save_key
#' function will save your key as a .Rda file which can then be called to when
#' using any other function.
#'
#' @param place Text string describing the place for the query
#'
#' @return A dataframe information on locations that share the name with the
#'   search location. Nine variables are returned: place_id, woeid, latitude,
#'   longitude, place_url, place_type, place_type_id, timezone, woe_name.

#' @export
#' @name findPlaces
#'
#' @examples
#' \dontrun{
#' find_place(place = "New York")
#'
#' find_place(place = "England")
#' }
find_place <- function(place = NULL) {
  if (is.null(place) == TRUE) {
    stop("provide a place")
  }

  # this checks for the presence of a key, if no key it prompts the user to create one,
  # it then checks the validity of the key
  api_key <- create_and_check_key()

  place <- gsub(" ", "+", trimws(place))

  place_url <- paste("https://api.flickr.com/services/rest/?method=flickr.places.find&api_key=", api_key, "&query=", place, sep = "")

  place_xml <- search_url(base_url = place_url)

  if (!is.null(place_xml)) {
    place_atts <- xml2::xml_find_all(place_xml, "//place", ns = xml2::xml_ns(place_xml))
    place_data <- dplyr::bind_rows(lapply(xml2::xml_attrs(place_atts), function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))
  }

  return(place_data)
}
