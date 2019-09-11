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
#' @param text Character, optional. A free text search. Photos who's title,
#'   description or tags contain the text will be returned. You can exclude
#'   results that match a term by prepending it with a - character. Free text
#'   searches for words in oder provided, for example a search for "climbing
#'   rock" will be different to "rock climbing"
#' @param tags Character, optional. A comma-delimited list of tags. Photos with
#'   one or more of the tags listed will be returned. You can exclude results
#'   that match a term by prepending it with a - character.
#' @param tags_any If TRUE, photos containing any of the tags will be returned.
#'   If FALSE, only photos containg all tags will be returned. Defulted to
#'   return any tags.
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
#' Full list of variables returned: id: photographs unique id number, owner: the
#' unique id of the Flickr user, secret: photograph unique secret number,
#' server: Flickr server data, farm: Flickr server data, title: photograph
#' title, ispublic: whether photograph is public; 1 = yes, 0 = no, isfriend
#' whether user is friend; 1 = yes, 0 = no, isfamily whether user is family; 1 =
#' yes, 0 = no, license: use licence of the image see \link{
#' https://www.flickr.com/services/api/flickr.photos.licenses.getInfo.html} for
#' details, datetaken: date and time of image capture, datetakengranularity:
#' accuracy of image date see \link{
#' https://www.flickr.com/services/api/misc.dates.html} for more information on
#' dates, datetakenunknown: whether date is unknown see \link{
#' https://www.flickr.com/services/api/misc.dates.html} for more information on
#' dates, count_views: number of view the photograph has had, count_comments:
#' number of comments on the photograph, count_faves: number of times the
#' photograph has been favourited, tags: user defined tags on the photograph,
#' latitude: latitude of where the image was taken, longitude: longitude of
#' where the image was taken, accuracy: accuracy of spatial reference see \link{
#' https://www.flickr.com/services/api/flickr.photos.search.html } for more
#' information, context: a numeric value representing the photo's geotagginess
#' beyond latitude and longitude \link{
#' https://www.flickr.com/services/api/flickr.photos.search.html } for more
#' information, place_id: unique numeric number representing the location of the
#' photograph, woeid: unique numeric number representing the location of the
#' photograph, geo_is_family: whether only friends can see geo; 1 = yes, 0 = no,
#' geo_is_friend: whether only family can see geo; 1 = yes, 0 = no,
#' geo_is_contact: whether only contact can see geo; 1 = yes, 0 = no,
#' geo_is_public whether geo is public; 1 = yes, 0 = no, url_sq: URL for square
#' image, height_sq: height for square image, width_sq: width for square image,
#' url_t : URL for square image thumbnail image 100 on longest side, height_t:
#' height for thumbnail image 100 on longest side, width_t: width for thumbnail
#' image 100 on longest side, url_s: URL for small square image 75x75, height_s:
#' height for small square image 75x75, width_s	: width for small square image
#' 75x75, url_q: URL for large square image 150x150, height_q: height for large
#' square image 150x150, width_q: width for large square image 150x150, url_m:
#' URL for small image 240 on longest side, height_m: height for small image 240
#' on longest side, width_m: width for small image 240 on longest side, url_n:
#' URL for small image 320 on longest side, height_n: height for small image 320
#' on longest side, width_n: width for small image 320 on longest side, url_z:
#' URL for medium image 640 on longest side, height_z: height for medium image
#' 640 on longest side, width_z: width for medium image 640 on longest side,
#' url_c: URL for medium image 800 on longest side, height_c: height for medium
#' image 800 on longest side, width_c: width for medium image 800 on longest
#' side, url_l: URL for large image 1024 on longest side, height_l: height for
#' large image 1024 on longest side, width_l: width for large image 1024 on
#' longest side, url_o: URL for original image, either a jpg, gif or png,
#' depending on source format, height_o: height for original image, either a
#' jpg, gif or png, depending on source format, width_o: width for original
#' image, either a jpg, gif or png, depending on source format.
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
           tags_any = TRUE,
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

    #specify tag mode
    if (isTRUE(tags_any)){

      tags_any <- "any"

    } else {

        tags_any <- "all"

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
        tag_mode = tags_any,
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

    if (is.null(pics)){

      stop("No photographs meeting criteria")

    }

    pics <- parse_pic(pics = pics)


    # end
    return(pics)
  }
