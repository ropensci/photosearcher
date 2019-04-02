#' photo_search
#'
#' @param min_taken String, minimum date of photograph for search provide as "YYYY-MM-DD".
#' @param max_taken String, maximum date of photograph for search provide as "YYYY-MM-DD".
#' @param text String, text to be searched.
#' @param bbox String, optional bounding box of search area provide as "minimum_longitude,minimum_latitude,maximum_longitude,maximum_latitude".
#' @param has_geo Logical, arguement for whether returned photos need to be georeference.
#'
#' @return Output will be a dataframe consisting of 54 variables including; lattitude and longitude of photograph, photograph tags and image urls
#' @export
#'
#' @examples
#' photo_search(min_taken = "2019-01-01", max_taken = "2019-01-02", text = "tree", bbox = "-13.623047,47.279229,3.251953,60.630102", has_geo = TRUE)
#'
#' photo_search(min_taken = "20001-01", max_taken = "2010-01-01", text = "mountain", bbox = NULL, has_geo = NULL)
photo_search <-
  function(min_taken = "2019-01-01",
             max_taken = "2019-01-01",
             text = NULL,
             bbox = NULL,
             has_geo = TRUE) {
    text <- gsub(" ", "+", trimws(text))
    mindate <- min_taken
    maxdate <- max_taken
    pics <- NULL
    spatial_df <- NULL

    # create dfs so large searches can be subset dynamically
    date_df <- data.frame(mindate = mindate, maxdate = maxdate)

    # start while loop - until all dates are looped through
    while (nrow(date_df) > 0) {

      # set search dates
      mindate <- date_df[1, "mindate"]
      maxdate <- date_df[1, "maxdate"]

      # rest page to 1
      i <- 1

      # url but new util doesnt work here
      base_url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", api_key,
        "&text=", text,
        "&min_taken_date=", as.character(mindate),
        "&max_taken_date=", as.character(maxdate),
        ifelse(!(is.null(bbox)), paste0("&bbox=", bbox), ""),
        ifelse(has_geo, paste0("&has_geo=", has_geo), ""),
        "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o,count_views,count_comments,count_faves",
        "&page=", i,
        "&format=", "rest",
        sep = ""
      )

      photo_xml <- search_url(base_url = base_url)

      if (!is.null(photo_xml)) {
        pages_data <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
        pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
        total_pages <- pages_data["pages", ]
        total <- pages_data["total", ]

        # if > 4000 and not single days, split
        if (total > 4000 && (as.Date(mindate) != (as.Date(maxdate) - 1))) {
          x <- ceiling(total / 4000)
          y <- length(seq(as.Date(mindate), as.Date(maxdate), by = "+1 day"))

          # if x + 1 is larger than number of days, split in to single days
          if (x + 1 > y) {
            dates <- seq(as.Date(mindate), as.Date(maxdate), by = "+1 day")
          }

          # else split accoring to x
          else {
            dates <- seq(as.Date(mindate), as.Date(maxdate), length.out = x + 1)
          }

          # create dataframe with minmaxdates
          date_df <- rbind(date_df[-1, ], data.frame(mindate = dates[1:(length(dates) - 1)], maxdate = dates[2:length(dates)]))
        }

        # if > 4000 and single days, pass days to be split by area
        else if (total > 4000 && (as.Date(mindate) == (as.Date(maxdate) - 1))) {
          spatial_df <- rbind(spatial_df, data.frame(mindate = mindate, maxdate = maxdate))

          date_df <- date_df[-1, ]
        }

        # if all conditions are satisfied get data
        else if (total <= 4000 && total_pages > 0) {

          # get data second error catch here
          pics_tmp <- NULL

          # loop thru pages of photos and save the list in a DF
          for (i in c(1:total_pages)) {

            # url but new util doesnt work here
            base_url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", api_key,
              "&text=", text,
              "&min_taken_date=", as.character(mindate),
              "&max_taken_date=", as.character(maxdate),
              ifelse(!(is.null(bbox)), paste0("&bbox=", bbox), ""),
              ifelse(has_geo, paste0("&has_geo=", has_geo), ""),
              "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o,count_views,count_comments,count_faves",
              "&page=", i,
              "&format=", "rest",
              sep = ""
            )

            # this new one works here
            photo_xml <- search_url(base_url = base_url)

            if (!is.null(photo_xml)) {
              photo_atts <- xml2::xml_find_all(photo_xml, "//photo", ns = xml2::xml_ns(photo_xml))
              tmp_df <- dplyr::bind_rows(lapply(xml2::xml_attrs(photo_atts), function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))

              pics_tmp <- dplyr::bind_rows(pics_tmp, tmp_df)
              tmp_df <- NULL
            }
          }

          pics <- dplyr::bind_rows(pics, pics_tmp)

          date_df <- date_df[-1, ]
        }

        # else search is empty, skip
        else {
          date_df <- date_df[-1, ]
        }
      }
    }

    # split single days by area if bbox is available
    if (!is.null(bbox) && !is.null(spatial_df)) {

      # add bbox to df
      strbbox <- unlist(strsplit(bbox, ","), use.names = FALSE)
      spatial_df$xmin <- strbbox[1]
      spatial_df$ymin <- strbbox[2]
      spatial_df$xmax <- strbbox[3]
      spatial_df$ymax <- strbbox[4]

      while (nrow(spatial_df) > 0) {
        mindate <- spatial_df[1, "mindate"]
        maxdate <- spatial_df[1, "maxdate"]
        xmin <- spatial_df[1, "xmin"]
        ymin <- spatial_df[1, "ymin"]
        xmax <- spatial_df[1, "xmax"]
        ymax <- spatial_df[1, "ymax"]

        # bbox for url search
        bbox <- paste(xmin, ",", ymin, ",", xmax, ",", ymax, sep = "")

        # reset page numbers
        i <- 1

        # url
        base_url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", api_key,
          "&text=", text,
          "&min_taken_date=", as.character(mindate),
          "&max_taken_date=", as.character(maxdate),
          ifelse(!(is.null(bbox)), paste0("&bbox=", bbox), ""),
          ifelse(has_geo, paste0("&has_geo=", has_geo), ""),
          "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o,count_views,count_comments,count_faves",
          "&page=", i,
          "&format=", "rest",
          sep = ""
        )

        # this new one works here
        photo_xml <- search_url(base_url = base_url)

        if (!is.null(photo_xml)) {
          pages_data <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
          pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
          total_pages <- pages_data["pages", ]
          total <- pages_data["total", ]

          # if less than 4000 and greater than 0
          if (total <= 4000 && total > 0) {

            # get data second error catch here
            pics_tmp <- NULL

            # loop thru pages of photos and save the list in a DF
            for (i in c(1:total_pages)) {

              # url
              base_url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", api_key,
                "&text=", text,
                "&min_taken_date=", as.character(mindate),
                "&max_taken_date=", as.character(maxdate),
                ifelse(!(is.null(bbox)), paste0("&bbox=", bbox), ""),
                ifelse(has_geo, paste0("&has_geo=", has_geo), ""),
                "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o,count_views,count_comments,count_faves",
                "&page=", i,
                "&format=", "rest",
                sep = ""
              )

              # this new one works here
              photo_xml <- search_url(base_url = base_url)

              if (!is.null(photo_xml)) {
                photo_atts <- xml2::xml_find_all(photo_xml, "//photo", ns = xml2::xml_ns(photo_xml))
                tmp_df <- dplyr::bind_rows(lapply(xml2::xml_attrs(photo_atts), function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))

                pics_tmp <- dplyr::bind_rows(pics_tmp, tmp_df)
                tmp_df <- NULL
              }
            }

            pics <- dplyr::bind_rows(pics, pics_tmp)

            date_df <- date_df[-1, ]
          }

          # if more than 4000 split
          else if (total > 4000 && (xmin != xmax | ymin != ymax)) {

            # find the right number of squares needed
            num <- ceiling(total / 4000)
            num_sqrt <- sqrt(num)
            num_next <- ceiling(num_sqrt)
            sqr_next <- num_next * num_next

            # number to sequence x and y coords by
            z <- sqrt(sqr_next) + 1

            xseq <- seq(xmin, xmax, length.out = z)
            yseq <- seq(ymin, ymax, length.out = z)

            xy_df <- NULL

            for (y in 1:(length(yseq) - 1)) {
              x_df <- data.frame(xmin = xseq[1:(length(xseq) - 1)], xmax = xseq[2:(length(xseq))])
              y_df <- data.frame(ymin = yseq[y], ymax = yseq[y + 1])

              z_df <- data.frame(mindate = mindate, maxdate = maxdate, xmin = x_df$xmin, ymin = y_df$ymin, xmax = x_df$xmax, ymax = y_df$ymax)
              xy_df <- rbind(xy_df, z_df)

              rm(list = "z_df")
            }

            spatial_df <- rbind(spatial_df[-1, ], xy_df)
          }

          # if bbox coords become points
          else if (total > 4000 && (xmin == xmax && ymin == ymax)) {

            # warn that single point > 4000 photos a day
            warning("Location ", bbox, " between dates", mindate, " and ", maxdate, " skipped as > 4000 returns")

            spatial_df <- rbind(spatial_df[-1, ])
          }

          # else if less than 1 skip date
          else {
            print("else")

            spatial_df <- rbind(spatial_df[-1, ])
          }
        }
      }
    }

    # end
    return(pics)
  }
