#' Get metadata for a single photo
#'
#' Returns image metadata for a single photograph.
#'
#'When running the function you need an API key saved as
#'photosearcher_key.sysdata in your working directory. If this is the first
#'function you run you will be prompted to create and enter your API key. The
#'API key will then be saved as photosearcher_key.sysdata in your working
#'directory and is used for all function.
#'
#' @param photo_id Character, required. The id of the photo to get information
#'   for.
#'
#' @return Dataframe of information on given image
#'
#' @family Get data for known photograph
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_photoinfo(photo_id = 48704764812)
#'}
get_photoinfo <- function(photo_id = NULL){

  # this checks for the presence of a key, if no key it prompts the user to
  # create one, it then checks the validity of the key
  api_key <- create_and_check_key()

  out <- NULL

  base_url <- paste("https://api.flickr.com/services/rest/",
                "?method=flickr.photos.getInfo&api_key=",
                api_key,
                "&photo_id=",
                photo_id,
                sep = "")

  photo_xml <- search_url(base_url = base_url)

  find_errors(error_xml = photo_xml)

  if (!is.null(photo_xml)){

    photo_atts <- xml2::xml_find_all(photo_xml,
                                     "//photo",
                                     ns = xml2::xml_ns(photo_xml))

    out <- dplyr::bind_rows(lapply(xml2::xml_attrs(photo_atts),
                                   function(x) data.frame(
                                     as.list(x), stringsAsFactors = FALSE)))

  }


  out <- data.frame(lapply(out, as.character), stringsAsFactors=FALSE)

  cols.num <- c("id",
                "server",
                "farm",
                "dateuploaded",
                "isfavorite",
                "license",
                "safety_level",
                "rotation",
                "views"
                )

  out[cols.num] <- sapply(out[cols.num],as.numeric)

  out$dateuploaded <- as.POSIXct(out$dateuploaded, origin="1970-01-01")

  return(out)

}
