#' get_photoinfo
#'
#' @param photo_id Character, required. The id of the photo to get information
#'   for.
#'
#' @return Dataframe of information on given image
#' @export
#'
#' @examples
#' \dontrun{
#' get_photoinfo(photo_id = 123)
#'}
get_photoinfo <- function(photo_id = NULL){

  # this checks for the presence of a key, if no key it prompts the user to
  # create one, it then checks the validity of the key
  api_key <- create_and_check_key()

  z <- paste("https://api.flickr.com/services/rest/",
                "?method=flickr.photos.getInfo&api_key=",
                api_key,
                "&photo_id=",
                photo_id,
                sep = "")

  photo_xml <- search_url(z)

  warn <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))

  if (warn[1,1] != photo_id){

    stop("Photo not found")

  } else {

    photo_atts <- xml2::xml_find_all(photo_xml,
                                    "//photo",
                                    ns = xml2::xml_ns(photo_xml))

    out <- dplyr::bind_rows(lapply(
      xml2::xml_attrs(photo_atts), function(x) data.frame(
        as.list(x), stringsAsFactors = FALSE)))
  }

  return(out)

}
