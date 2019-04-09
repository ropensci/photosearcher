download_images <- function(photo_id = NULL, saveDir = "downloaded_images"){

  if (is.null(photo_id) == TRUE) {
    stop("provide a photo id")
  }

  api_key <- as.character(get_key())

  if (!dir.exists(saveDir)) {
    message(paste("saveDir", saveDir, "does not exist, I will create it for you"))
    dir.create(saveDir, recursive = TRUE)
  }

  for (i in 1:length(photo_id)) {

    photo_image <- photo_id[i]

    z <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.getSizes&api_key=",api_key,"&photo_id=",photo_image,sep="")

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
