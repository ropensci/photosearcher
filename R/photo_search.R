#' Search for photo metadata
#'
#' Returns image metadata for photos matching the search terms.
#'
#' Uses the flickr.photos.search API method from the Flickr API. This search method
#' requires a limiting factor to prevent parameterless searches - to enure this is met the
#' function requires both a minimum and a maximum date that searched photographs were
#' taken on. See \url{https://www.flickr.com/services/api/flickr.photos.search.html} for
#' more information on the API method.
#'
#' @param mindate character. Minimum date of photograph for search provided as
#'   "YYYY-MM-DD".
#' @param maxdate character. Maximum date of photograph for search provided as
#'   "YYYY-MM-DD".
#' @param text character. Optional text to be searched.
#' @param tags character. Optional tags to filter by.
#' @param bbox character. Optional bounding box of search area provide as:
#'   \code{minimum_longitude, minimum_latitude, maximum_longitude, maximum_latitude}.
#' @param has_geo logical. Optional arguement for whether returned photos need
#'   to be georeference.
#'
#' @return data.frame. Output consists of 57 variables including;
#'   latitude and longitude of photograph, date and time it was taken,
#'   associated tags and image urls.
#'
#'   Full list of variables returned: id, owner, secret, server, farm, title,
#'   ispublic, isfriend, isfamily, license, datetaken, datetakengranularity,
#'   datetakenunknown, count_views, count_comments, count_faves, tags, latitude,
#'   longitude, accuracy, context, place_id, woeid, geo_is_family,
#'   geo_is_friend, geo_is_contact, geo_is_public, url_sq, height_sq, width_sq,
#'   url_t, height_t, width_t, url_s, height_s,	width_s	url_q, height_q,
#'   width_q, url_m, height_m, width_m, url_n, height_n, width_n, url_z,
#'   height_z, width_z, url_c, height_c, width_c, url_l, height_l, width_l,
#'   url_o, height_o, width_o.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' photo_search(
#'   mindate = "2019-01-01",
#'   maxdate = "2019-01-02",
#'   text = "tree",
#'   bbox = "-13.623047,47.279229,3.251953,60.630102",
#'   has_geo = TRUE
#' )
#'
#'
#' photo_search(
#'   mindate = "2019-01-01",
#'   maxdate = "2019-01-02",
#'   text = "tree",
#'   bbox = "-13.623047,47.279229,3.251953,60.630102",
#'   has_geo = TRUE
#' )
#' }
#'
photo_search <-
  function(mindate = "2019-01-01",
             maxdate = "2019-01-01",
             text = NULL,
             tags = NULL,
             bbox = NULL,
             has_geo = TRUE) {
    text <- gsub(" ", "+", trimws(text))
    tags <- gsub(" ", "+", trimws(tags))
    tags <- paste(tags, collapse = ",")
    pics <- NULL


    # create dfs so large searches can be subset dynamically
    date_df <- data.frame(mindate = mindate, maxdate = maxdate)

    # this checks for the presence of a key, if no key it prompts the user to create one,
    # it then checks the validity of the key
    api_key <- create_and_check_key()

    #check for vailid bbox
    if (!is.null(bbox)){
      check_bbox(bb = bbox, key = api_key)
    }

    # start while loop - until all dates are looped through
    while (nrow(date_df) > 0) {

      # set search dates
      mindate <- date_df[1, "mindate"]
      maxdate <- date_df[1, "maxdate"]

      # rest page to 1
      i <- 1

      base_url <- get_url(
        mindate = mindate,
        maxdate = maxdate,
        api_key = api_key,
        page = i,
        text = text,
        tags = tags,
        bbox = bbox,
        has_geo = has_geo
      )

      photo_xml <- search_url(base_url = base_url)

      if (!is.null(photo_xml)) {
        pages_data <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
        pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
        total_pages <- pages_data["pages", ]
        total <- pages_data["total", ]

        # if > 4000 and not single days, split
        if ((total > 4000 && (as.Date(mindate)) != (as.Date(maxdate) - 1)) | (total > 4000 && (as.Date(mindate)) != (as.Date(maxdate)))) {
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
        else if ((total > 4000 && (as.Date(mindate)) == (as.Date(maxdate) - 1)) | (total > 4000 && (as.Date(mindate)) == (as.Date(maxdate)))) {

          warning("Dates ", mindate, " to ", maxdate, "skipped: too many API results")

          date_df <- date_df[-1, ]
        }

        # if all conditions are satisfied get data
        else if (total <= 4000 && total_pages > 0) {

          # get data second error catch here
          pics_tmp <- NULL

          # loop thru pages of photos and save the list in a DF
          for (i in c(1:total_pages)) {
            base_url <- get_url(
              mindate = mindate,
              maxdate = maxdate,
              api_key = api_key,
              page = i,
              text = text,
              tags = tags,
              bbox = bbox,
              has_geo = has_geo
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

    #end
    return(pics)
  }
