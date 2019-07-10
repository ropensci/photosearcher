#' download_images
#'
#' Downloads images based on their Flickr ID. Uses the flickr.photos.getSizes API method
#' from the Flickr API to test whether you have permission to download an image. See
#' \url{https://www.flickr.com/services/api/flickr.photos.getSizes.html} for more
#' information on the API method. If permission is available the image is downloaded and
#' saved as a .jpeg in a given save directory.
#'
#' @param photo_id numeric. id of photo to dowload, can be single id, list or column for
#'   photo_search outputs
#' @param saveDir character. name of directory for photos to be saved in. This will be
#'   created if it doesn't exist.
#'
#' @param max_image_height numeric. maximum number of pixels for images height
#'
#' @param max_image_width numeric. maximum number of pixels for images width
#' #'
#' @param quiet logical. If TRUE, suppress status messages (if any), and the progress bar.
#'
#' @return character. A vector of the images that the user does not have permission to
#'   download. Images will be saved to \code{saveDir}.
#' @export
#'
#' @examples
#' \dontrun{
#' download_images(photo_id = 123, saveDir = "images")
#'
#' download_images(photo_id = photo_search_outputs$id, saveDir = "downloaded_photos", quiet = TRUE)
#'
#' download_images(photo_id = 123, maximum_image_height = 1200, maximum_image_width = 1200, )
#' }
download_images <- function(photo_id, saveDir = "downloaded_images", max_image_height = NULL, max_image_width = NULL, quiet = FALSE) {

  # this checks for the presence of a key, if no key it prompts the user to create one,
  # it then checks the validity of the key
  api_key <- create_and_check_key()

  # create saveDir
  if (!dir.exists(saveDir)) {
    ui_info(paste("saveDir", saveDir, "does not exist, I will create it for you"))
    dir.create(saveDir, recursive = TRUE)
  }

  downloadable <- dplyr::bind_rows(lapply(photo_id, function(x) download_image_single(x, saveDir, api_key, max_image_height, max_image_width, quiet)))


  return(downloadable)
}


#' @noRd
download_image_single <- function(photo_id, saveDir, api_key, max_image_height, max_image_width, quiet) {
  can_download <- NULL

  z <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.getSizes&api_key=", api_key, "&photo_id=", photo_id, sep = "")

  photo_xml <- search_url(z)

  if (!is.null(photo_xml)) {
    download_atts <- xml2::xml_find_all(photo_xml, "//sizes", ns = xml2::xml_ns(photo_xml))

    tmp_df <- dplyr::bind_rows(lapply(xml2::xml_attrs(download_atts), function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))

    if (!is.null(tmp_df$candownload)){

      if ((tmp_df$candownload) == 0) {

        out <- data.frame(id = photo_id, can_download = 0, stringsAsFactors = FALSE)

      } else {

        out <- data.frame(id = photo_id, can_download = 1, stringsAsFactors = FALSE)

        photo_url <- xml2::xml_find_all(photo_xml, "//size", ns = xml2::xml_ns(photo_xml))

        tmp_df <- dplyr::bind_rows(lapply(xml2::xml_attrs(photo_url), function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))

        # choose image size (height)
        if (!is.null(max_image_height)) {
          tmp_df <- tmp_df[!(as.numeric(tmp_df$height) > as.numeric(max_image_height)), ]
        }

        # choose image size (width)
        if (!is.null(max_image_width)) {
          tmp_df <- tmp_df[!(as.numeric(tmp_df$width) > as.numeric(max_image_width)), ]
        }


        # download biggest possible image
        to_download <- tmp_df$source[nrow(tmp_df)]

        utils::download.file(
          url = to_download,
          destfile = file.path(saveDir, basename(to_download)),
          mode = "wb",
          quiet = quiet
        )
      }

    } else {

      out <- data.frame(id = photo_id, can_download = 0, stringsAsFactors = FALSE)}
  }

  return(out)

}
