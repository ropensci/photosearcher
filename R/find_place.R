#' Find place location data
#'
#' Find the WoeID and other location data for a given place
#'
#' Takes user defined location and returns location data for the search. Uses
#' the flickr.places.find API method from the Flickr API. See
#' \url{https://www.flickr.com/services/api/flickr.places.find.html} for more
#' information on the API method.
#'
#'When running the function you need an API key saved as
#'photosearcher_key.sysdata in your working directory. If this is the first
#'function you run you will be prompted to create and enter your API key. The
#'API key will then be saved as photosearcher_key.sysdata in your working
#'directory and is used for all function.
#'
#'
#' @param place character. The place for the query
#'
#' @return data.frame. Information on locations that share the name with the
#'   search location. Nine variables are returned: place_id, woe_id, latitude,
#'   longitude, place_url, place_type, place_type_id, timezone, woe_name.
#'
#' @family Information about places
#'
#' @export
#' @name findPlaces
#'
#' @examples
#' \dontrun{
#' find_place(place = "New York")
#'
#' find_place(place = "England")
#' }
find_place <- function(place) {

  # this checks for the presence of a key, if no key it prompts the user to
  # create one, it then checks the validity of the key
  api_key <- create_and_check_key()

  #check flickr location services are working
  check_location(api_key = api_key)

  place <- gsub(" ", "+", trimws(place))

  place_url <- paste("https://api.flickr.com/services/rest/",
                     "?method=flickr.places.find&api_key=",
                     api_key,
                     "&query=",
                     place,
                     sep = "")

  place_xml <- search_url(base_url = place_url)

  find_errors(error_xml = place_xml)

  if (!is.null(place_xml)) {
    place_atts <- xml2::xml_find_all(
      place_xml, "//place", ns = xml2::xml_ns(place_xml))

    place_data <- dplyr::bind_rows(lapply(
      xml2::xml_attrs(place_atts), function(x) data.frame(
        as.list(x), stringsAsFactors = FALSE)))
  }

  return(place_data)
}



