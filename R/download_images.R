#' Download images
#'
#' Downloads images based on their Flickr ID. Uses the flickr.photos.getSizes
#' API method from the Flickr API to test whether you have permission to
#' download an image. See
#' \url{https://www.flickr.com/services/api/flickr.photos.getSizes.html} for
#' more information on the API method. If permission is available the image is
#' downloaded and saved as a .jpeg in a given save directory.
#'
#' @param photo_id numeric or character vector. id of photo to download, can be
#'   single id, list or column for photo_search outputs
#' @param save_dir character. name of directory for photos to be saved in. This
#'   will be created if it doesn't exist.
#'
#' @param max_image_height numeric. maximum number of pixels for images height
#'
#' @param max_image_width numeric. maximum number of pixels for images width #'
#' @param quiet logical. If TRUE, suppress status messages (if any), and the
#'   progress bar.
#'
#' @return character. A vector of the images that the user does not have
#'   permission to download. Images will be saved to \code{save_dir}.
#' @export
#'
#' @examples
#' \dontrun{
#' download_images(photo_id = 48704764812, save_dir = "images")
#'
#' download_images(photo_id = 48704764812, maximum_image_height = 1200,
#' maximum_image_width = 1200, save_dir = "smaller_images") }
download_images <- function(photo_id,
                            save_dir = NULL,
                            max_image_height = NULL,
                            max_image_width = NULL,
                            quiet = FALSE) {

  # this checks for the presence of a key, if no key it prompts the user to
  # create one, it then checks the validity of the key
  api_key <- create_and_check_key()

  if (is.null(save_dir)){

    stop("Please supply a save directory")

  }

  #not sure how to print this first?
  warning(paste(length(photo_id),
                " photos to be downloaded may take in excess of ",
                (length(photo_id) * 10),
                " seconds",
                sep = ""))

  # create save_dir
  if (!dir.exists(save_dir)) {
    ui_info(paste("save_dir",
                  save_dir, "does not exist, I will create it for you"))
    dir.create(save_dir, recursive = TRUE)
  }

  downloadable <- dplyr::bind_rows(
    lapply(photo_id, function(x) download_image_single(
      x, save_dir, api_key, max_image_height, max_image_width, quiet)))


  return(downloadable)
}


#' @noRd
download_image_single <- function(photo_id,
                                  save_dir,
                                  api_key,
                                  max_image_height,
                                  max_image_width, quiet) {
  can_download <- NULL

  z <- paste("https://api.flickr.com/services/rest/",
             "?method=flickr.photos.getSizes&api_key=",
             api_key,
             "&photo_id=",
             photo_id,
             sep = "")

  photo_xml <- search_url(z)

  warn <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))

  if (warn[2, ] == "Photo not found"){

    out <- data.frame(id = photo_id,
                      can_download = "Photo not found",
                      stringsAsFactors = FALSE)

  } else {

  if (!is.null(photo_xml)) {

    download_atts <- xml2::xml_find_all(photo_xml,
                                        "//sizes",
                                        ns = xml2::xml_ns(photo_xml))

    tmp_df <- dplyr::bind_rows(
      lapply(xml2::xml_attrs(download_atts),
             function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))


      if ((tmp_df$candownload) == 1) {

        photo_url <- xml2::xml_find_all(photo_xml,
                                        "//size",
                                        ns = xml2::xml_ns(photo_xml))

        tmp_df <- dplyr::bind_rows(lapply(
          xml2::xml_attrs(photo_url),
          function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))

        # choose image size (height)
        if (!is.null(max_image_height)) {
          tmp_df <- tmp_df[!(as.numeric(tmp_df$height) >
                               as.numeric(max_image_height)), ]
        }

        # choose image size (width)
        if (!is.null(max_image_width)) {
          tmp_df <- tmp_df[!(as.numeric(tmp_df$width) >
                               as.numeric(max_image_width)), ]
        }

        if (nrow(tmp_df) > 0){


          out <- data.frame(id = photo_id,
                            can_download = "Yes",
                            stringsAsFactors = FALSE)

          # download biggest possible image
          to_download <- tmp_df$source[nrow(tmp_df)]

          check_file <- gsub(".*/", "", to_download)

          check_file2 <- (paste(".\\",save_dir, "\\", check_file, sep = ""))

          if (file.exists(check_file2)){

            warning(paste(check_file, " exists, file will be overwritten"))


          }

          utils::download.file(
            url = to_download,
            destfile = file.path(save_dir, basename(to_download)),
            mode = "wb",
            quiet = quiet
          )

        } else {

          out <- data.frame(id = photo_id,
                            can_download = "No photos meeting size criteria",
                            stringsAsFactors = FALSE)

        }


      } else {

      out <- data.frame(id = photo_id,
                        can_download = "No",
                        stringsAsFactors = FALSE)
      }
   }

  }

  return(out)

}
