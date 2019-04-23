#' User_info
#'
#' Takes user defined FLickr user ID and returns data regarding their profile.
#' Uses the flickr.profile.getProfile API method from the Flickr API. See
#' \url{https://www.flickr.com/services/api/flickr.profile.getProfile.html} for
#' more information on the API method.
#'
#' Note: if this is the first function of the package you use you will be
#' prompted to enter your API key. API keys are avialable from
#' \url{https://www.flickr.com/services/apps/create/apply}. The API key will
#' then be saved as a .Rda file and be called to when using any other function.
#'
#' @param user_id String, the id of the user you wihs to obtain information for.
#'
#' @return Dataframe of 19 variables from the searched users publically
#'   available infromation including; hometown and occupation. Full list of
#'   returned variables: id, nsid	join_date, occupation, hometown, showcase_set,
#'   showcase_set_title, first_name, last_name, profile_description,
#'   website	city, country, facebook, twitter, tumblr, Instagram, pinterest,
#'   email
#' @export
#'
#' @examples
#' \dontrun{
#' user_info(user_id = "8017836@N02")
#'
#' user_info(user_id = c("11763518@N00", "39745335@N06", "104939923@N02"))
#' }
user_info <- function(user_id = NULL) {
  if (is.null(user_id) == TRUE) {
    stop("provide user id")
  }

  api_key <- as.character(get_key())

  id_tmp <- NULL
  id_info <- NULL

  for (i in user_id) {
      get_info <- paste("https://api.flickr.com/services/rest/?method=flickr.profile.getProfile&api_key=", api_key, "&user_id=", i, sep = "")

      user_xml <- search_url(base_url = get_info)

      if (!is.null(user_xml)) {
        user_atts <- xml2::xml_find_all(user_xml, "//profile", ns = xml2::xml_ns(user_xml))
        tmp_df <- dplyr::bind_rows(lapply(xml2::xml_attrs(user_atts), function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))

        id_tmp <- dplyr::bind_rows(id_tmp, tmp_df)
        tmp_df <- NULL
      }
    }

    id_info <- dplyr::bind_rows(id_info, id_tmp)


  return(id_info)
}
