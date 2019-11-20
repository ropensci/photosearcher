#'Get Exif data
#'
#'Returns Exchangeable image file format data for a single photograph. For more
#'information on how Exif data differs from metadata see:
#'\url{https://www.flickr.com/services/api/flickr.photos.getExif.html}
#'
#'When running the function you need an API key saved as
#'photosearcher_key.sysdata in your working directory. If this is the first
#'function you run you will be prompted to create and enter your API key. The
#'API key will then be saved as photosearcher_key.sysdata in your working
#'directory and is used for all function.
#'
#'@param photo_id Id of photograph
#'
#'@return A dataframe of "exchangeable image file format" information for the
#'  given photograph
#'
#'@family Get data for known photograph
#'
#'@export
#'
#' @examples
#' \dontrun{
#' get_exif(photo_id = 48704764812)
#'}
get_exif <- function(photo_id = NULL){

  # this checks for the presence of a key, if no key it prompts the user to
  # create one, it then checks the validity of the key
  api_key <- create_and_check_key()

  exif <- paste("https://api.flickr.com/services/rest/",
                "?method=flickr.photos.getExif&api_key=",
                api_key,
                "&photo_id=",
                photo_id,
                sep = "")

  exif_xml <- search_url(base_url = exif)

  find_errors(error_xml = exif_xml)

  if (!is.null(exif_xml)) {

      exif_atts <- xml2::xml_find_all(exif_xml, "//photo",
                                      ns = xml2::xml_ns(exif_xml))

      exif_df <- NULL
      tmp_df <- NULL
      exif_length <- as.numeric(xml2::xml_length(exif_atts))

      for (i in 1:exif_length){

        heading <- as.character(xml2::xml_attrs(
          xml2::xml_child(exif_atts[[1]], i))[4])

        cell <- as.character(xml2::xml_child(
          xml2::xml_child(exif_atts[[1]], 1), 1))

        tmp_df<- data.frame(cell)
        names(tmp_df)[names(tmp_df) == "cell"] <- paste(heading)

        exif_df <- dplyr::bind_cols(exif_df, tmp_df)

        rm(tmp_df)

      }

    }

  return(exif_df)

}
