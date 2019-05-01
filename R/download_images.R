#' download_images
#'
#' Downloads images based on their FLickr id. Uses the flickr.photos.getSizes
#' API method from the Flickr API to test whether you have permission to
#' download an image. See
#' \url{https://www.flickr.com/services/api/flickr.photos.getSizes.html} for more
#' information on the API method. If permission is available the image is
#' downloaded and saved as a .jpeg in a given save directory.
#'
#' Note: if this is the first function of the package and you do not enter you
#' API key in the arguement api_key you use you will be prompted to enter your
#' API key or save it using the save_key function. API keys are avialable from
#' \url{https://www.flickr.com/services/apps/create/apply}. Using the save_key
#' function will save your key as a .Rda file which can then be called to when
#' using any other function.
#'
#' @param photo_id id of pgoto to dowload, can be single id, list or column for
#'   phot_search outputs
#' @param saveDir name of directory for photos to be saved in - if it doesnt
#'   exisit it will be created
#' @param api_key String, if you have used the save_key function the api_key
#'   argument is automatically filled. If not api_key can be used optionally to
#'   supplying your API key if you do not wish for it to be saved in the
#'   environment or as a .Rda
#'
#' @return jpeg image saved as the name of the photo id in given save directory
#' @export
#'
#' @examples
#' \dontrun{
#' download_images(photo_id = 123, saveDir = "images")
#'
#' download_images(photo_id = photo_search_outputs$id, saveDir = "downloaded_photos")
#' }
download_images <- function(photo_id = NULL, saveDir = "downloaded_images", api_key = NULL) {
  if (is.null(photo_id) == TRUE) {
    stop("provide a photo id")
  }

  # get or save the api_key
  if (is.null(api_key)) {
    stop("Enter API key or save using the save_key function")
  }

  #check for vailid key
  check_key(key = api_key)

  #create saveDir
  if (!dir.exists(saveDir)) {
    message(paste("saveDir", saveDir, "does not exist, I will create it for you"))
    dir.create(saveDir, recursive = TRUE)
  }

  for (i in 1:length(photo_id)) {
    photo_image <- photo_id[i]

    z <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.getSizes&api_key=", api_key, "&photo_id=", photo_image, sep = "")

    photo_xml <- search_url(z)

    if (!is.null(photo_xml)) {
      download_atts <- xml2::xml_find_all(photo_xml, "//sizes", ns = xml2::xml_ns(photo_xml))

      tmp_df <- dplyr::bind_rows(lapply(xml2::xml_attrs(download_atts), function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))

      if ((tmp_df$candownload) == 0) {
        warning("No permission to download image ", photo_image)
      } else {
        photo_url <- xml2::xml_find_all(photo_xml, "//size", ns = xml2::xml_ns(photo_xml))

        tmp_df <- dplyr::bind_rows(lapply(xml2::xml_attrs(photo_url), function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))

        to_download <- tmp_df$source[nrow(tmp_df)]

        utils::download.file(
          url = to_download,
          destfile = file.path(saveDir, basename(to_download)),
          mode = "wb"
        )
      }
    }
  }
}
