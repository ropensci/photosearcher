#' Title
#'
#' @param user_id String, the id of the user you wihs to obtain information for.
#'
#' @return Dataframe of users publically available infromation including;
#'   hometown and occupation
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

  if (length(user_id) > 0) {
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
  }

  return(id_info)
}
