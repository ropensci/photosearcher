#' get_exif
#'
#' @param photo_id Id of photograph
#'
#' @return A dataframe of "exchangeable image file format" information for the
#'   given photograph
#' @export
#'
#' @examples
#' \dontrun{
#' get_exif(photo_id = 123)
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

  warn <- data.frame(xml2::xml_attrs(xml2::xml_children(exif_xml)))

  if (warn[2, ] == "Photo not found"){

    stop("Photo not found")

  } else {

    if (!is.null(exif_xml)) {

      exif_atts <- xml2::xml_find_all(exif_xml, "//photo",
                                      ns = xml2::xml_ns(exif_xml))

      exif_df <- NULL
      tmp_df <- NULL
      exif_length <- as.numeric(xml_length(exif_atts))

      for (i in 1:exif_length){

        heading <- as.character(xml_attrs(xml_child(exif_atts[[1]], i))[4])
        cell <- as.character(xml_child(xml_child(exif_atts[[1]], i), 1))

        tmp_df<- data.frame(cell)
        names(tmp_df)[names(tmp_df) == "cell"] <- paste(heading)

        exif_df <- dplyr::bind_cols(exif_df, tmp_df)

        rm(tmp_df)

      }

    }

  }

  return(exif_df)

}
