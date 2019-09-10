#' Search for photo metadata
#'
#' Returns image metadata for photos matching the search terms.
#'
#' Uses the flickr.photos.search API method from the Flickr API. This search
#' method requires a limiting factor to prevent parameterless searches - to
#' ensure this is met the function requires both a minimum and a maximum date
#' that searched photographs were taken on. See
#' \url{https://www.flickr.com/services/api/flickr.photos.search.html} for more
#' information on the API method.
#'
#' @param mindate_taken Character, required. Minimum taken date. Photos with an
#'   taken date greater than or equal to this value will be returned. The date
#'   should be in the form of "YYYY-MM-DD".
#' @param maxdate_taken Character, required. Maximum taken date. Photos with an
#'   taken date less than or equal to this value will be returned. The date
#'   should be in the form of "YYYY-MM-DD".
#' @param text String, optional. A free text search. Photos who's title,
#'   description or tags contain the text will be returned. You can exclude
#'   results that match a term by prepending it with a - character.
#' @param tags String, optional. A comma-delimited list of tags. Photos with one
#'   or more of the tags listed will be returned. You can exclude results that
#'   match a term by prepending it with a - character.
#' @param bbox String, optional bounding box of search area provide as:
#'   "minimum_longitude,minimum_latitude,maximum_longitude,maximum_latitude".
#' @param woe_id Numeric, optional "where on earth identifier" can be supplied
#'   instead of bbox. Use \code{\link{find_place}} to obtain woe_id for a place.
#' @param sf_layer A simple features layer, optional, area to search for photos,
#'   can be supplied instead of a bbox or woeID.
#' @param has_geo Logical, optional argument for whether returned photos need
#'   associated spatial data.
#' @param mindate_uploaded Character, optional. Minimum upload date. Photos with
#'   an upload date greater than or equal to this value will be returned. The
#'   date can be in the form of a unix timestamp or mysql datetime.
#' @param maxdate_uploaded Character, optional. Maximum upload date. Photos with
#'   an upload date less than or equal to this value will be returned. The date
#'   can be in the form of a unix timestamp or mysql datetime.
#' @param user_id Character, optional. The NSID of the user who's photo to
#'   search. If this parameter isn't passed then everybody's public photos will
#'   be searched.
#'
#' @return data.frame. Output consists of 57 variables including; latitude and
#'   longitude of photograph, date and time it was taken, associated tags and
#'   image urls.
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
#'   mindate_taken = "2019-01-01",
#'   maxdate_taken = "2019-01-02",
#'   text = "tree",
#'   bbox = "-13.623047,47.279229,3.251953,60.630102",
#'   has_geo = TRUE
#'   )
#'
#' photo_search(
#'   mindate_taken = "2017-01-01",
#'   maxdate_taken = "2017-01-02",
#'   mindate_uploaded = "2017-03-04",
#'   maxdate_uploaded = "2017-05-05",
#'   tags = "lake"
#'   )
#' }
photo_search <-
  function(mindate_taken = "2019-01-01",
           maxdate_taken = "2019-01-01",
           mindate_uploaded = NULL,
           maxdate_uploaded = NULL,
           user_id = NULL,
           text = NULL,
           tags = NULL,
           bbox = NULL,
           woe_id = NULL,
           sf_layer = NULL,
           has_geo = TRUE) {
    text <- gsub(" ", "+", trimws(text))
    tags <- gsub(" ", "+", trimws(tags))
    tags <- paste(tags, collapse = ",")
    pics <- NULL


    # create dfs so large searches can be subset dynamically
    date_df <- data.frame(mindate_taken = mindate_taken,
                          maxdate_taken = maxdate_taken)

    # this checks for the presence of a key, if no key it prompts the user to
    # create one, it then checks the validity of the key
    api_key <- create_and_check_key()

    # check that only one search location is given
    if ((!is.null(bbox) & !is.null(woe_id)) |
        (!is.null(sf_layer) & !is.null(woe_id)) |
        (!is.null(bbox) & !is.null(sf_layer))) {
      stop("Specify search location as only one of: woe_id, bbox or sf_layer.")
    }

    # change sf_layer to bbox
    if (!is.null(sf_layer)) {

      bbox <- create_bbox(sf_layer = sf_layer)

    }

    # check flickr location services work
    if (!is.null(woe_id)) {
      check_location(api_key = api_key)
    }

    # start while loop - until all dates are looped through
    while (nrow(date_df) > 0) {

      # set search dates
      mindate_taken <- as.POSIXct(date_df[1, "mindate_taken"])
      maxdate_taken <- as.POSIXct(date_df[1, "maxdate_taken"])

      # rest page to 1
      i <- 1

      base_url <- get_url(
        mindate_taken = mindate_taken,
        maxdate_taken = maxdate_taken,
        mindate_uploaded = mindate_uploaded,
        maxdate_uploaded = maxdate_uploaded,
        user_id = user_id,
        api_key = api_key,
        page = i,
        text = text,
        tags = tags,
        bbox = bbox,
        woe_id = woe_id,
        has_geo = has_geo
      )

      photo_xml <- search_url(base_url = base_url)

      find_errors(error_xml = photo_xml)

      if (!is.null(photo_xml)) {
        pages_data <- data.frame(
          xml2::xml_attrs(xml2::xml_children(photo_xml)))
        pages_data[] <- lapply(
          pages_data, FUN = function(x) as.integer(as.character(x)))
        total_pages <- pages_data["pages", ]
        total <- pages_data["total", ]

        # if total > 4000 and dates are not 1 second apart, split
        if (total > 4000 && (seq(
          mindate_taken, length.out = 2, by = "1 secs")[2] != maxdate_taken)) {
          x <- ceiling(total / 4000)

          dates <- seq(
            as.POSIXct(mindate_taken),
            as.POSIXct(maxdate_taken), length.out = x + 1)

          # create dataframe with minmaxdate_takens
          date_df <- rbind(
            date_df[-1, ],
            data.frame(mindate_taken = dates[1:(length(dates) - 1)],
                                      maxdate_taken = dates[2:length(dates)]))
        }

        # if > 4000 and single seconds skip
        else if (total > 4000 && (
          seq(mindate_taken,
              length.out = 2, by = "1 secs")[2] == maxdate_taken)) {
          warning(mindate_taken, " skipped: too many API results")

          date_df <- date_df[-1, ]
        }

        # if all conditions are satisfied get data
        else if (total <= 4000 && total_pages > 0) {

          # get data second error catch here
          pics_tmp <- NULL

          # loop thru pages of photos and save the list in a DF
          for (i in c(1:total_pages)) {
            base_url <- get_url(
              mindate_taken = mindate_taken,
              maxdate_taken = maxdate_taken,
              api_key = api_key,
              page = i,
              text = text,
              tags = tags,
              bbox = bbox,
              woe_id = woe_id,
              has_geo = has_geo
            )

            # this new one works here
            photo_xml <- search_url(base_url = base_url)

            if (!is.null(photo_xml)) {
              photo_atts <- xml2::xml_find_all(
                photo_xml, "//photo", ns = xml2::xml_ns(photo_xml))
              tmp_df <- dplyr::bind_rows(lapply(
                xml2::xml_attrs(photo_atts), function(x) data.frame(
                  as.list(x), stringsAsFactors = FALSE)))

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

    # is using sf_layer clip results to layer
    if (!is.null(sf_layer)) {
      with_geom <- sf::st_as_sf(pics,
                                coords = c("longitude", "latitude"),
                                crs = 4326)

      pics <- cbind(with_geom,
                    longitude = pics$longitude,
                    latitude = pics$latitude)

      sf_layer <- sf::st_transform(
        sf_layer, crs = "+proj=longlat +datum=WGS84 +no_defs")

      pics <- sf::st_intersection(pics, sf_layer)
    }


    # end
    return(pics)
  }
