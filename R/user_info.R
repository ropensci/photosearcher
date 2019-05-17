#' Get user information
#'
#' Takes Flickr user ID and returns the profile data.
#'
#' Uses the flickr.profile.getProfile API method from the Flickr API. See
#' \url{https://www.flickr.com/services/api/flickr.profile.getProfile.html} for
#' more information on the API method.
#'
#' @param user_id character. The id of the user you wish to obtain information for.
#'
#' @return data.frame. Dataframe of 18 variables from the searched users publically
#'   available information: id, nsid,	join_date, occupation, hometown, showcase_set,
#'   showcase_set_title, first_name, last_name, profile_description,
#'   website	city, country, facebook, twitter, tumblr, Instagram, pinterest
#'
#' @export
#'
#' @examples
#' \dontrun{
#' user_info(user_id = "8017836@N02")
#'
#' user_info(user_id = c("11763518@N00", "39745335@N06", "104939923@N02"))
#' }
user_info <- function(user_id) {

  # this checks for the presence of a key, if no key it prompts the user to create one,
  # it then checks the validity of the key
  api_key <- create_and_check_key()

  id_info <- dplyr::bind_rows(lapply(user_id, function(x) user_info_single(x, api_key)))

  return(id_info)
}

#' @noRd
# get a single user's info (for use in a vectorised function)
user_info_single <- function(user_id, api_key) {
  get_info <- paste("https://api.flickr.com/services/rest/?method=flickr.profile.getProfile&api_key=", api_key, "&user_id=", user_id, sep = "")

  user_xml <- search_url(base_url = get_info)

  if (!is.null(user_xml)) {
    user_atts <- xml2::xml_find_all(user_xml, "//profile", ns = xml2::xml_ns(user_xml))
    out <- dplyr::bind_rows(lapply(xml2::xml_attrs(user_atts), function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))
  }
  else {
    out <- NULL
  }
  return(out)
}
