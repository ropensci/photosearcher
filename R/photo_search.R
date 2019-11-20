#'Search for photo metadata
#'
#'Returns image metadata for photos matching the search terms.
#'
#'Uses the flickr.photos.search API method from the Flickr API. This search
#'method requires a limiting factor to prevent parameterless searches - to
#'ensure this is met the function requires both a minimum and a maximum date
#'that searched photographs were taken on. See
#'\url{https://www.flickr.com/services/api/flickr.photos.search.html} for more
#'information on the API method.
#'
#'When running the function you need an API key saved as
#'photosearcher_key.sysdata in your working directory. If this is the first
#'function you run you will be prompted to create and enter your API key. The
#'API key will then be saved as photosearcher_key.sysdata in your working
#'directory and is used for all function.
#'
#'@param mindate_taken Character, or date required. Minimum taken date. Photos
#'  with an taken date greater than or equal to this value will be returned. The
#'  date should be in the form of "YYYY-MM-DD".
#'@param maxdate_taken Character, or date required. Maximum taken date. Photos
#'  with an taken date less than or equal to this value will be returned. The
#'  date should be in the form of "YYYY-MM-DD".
#'@param text Character, optional. A free text search. Photos who's title,
#'  description or tags contain the text will be returned. You can exclude
#'  results that match a term by prepending it with a - character. Free text
#'  searches for words in order provided, for example a search for "climbing
#'  rock" will be different to "rock climbing".
#'@param tags Character vector, optional. A comma-delimited list of tags. Photos
#'  with one or more of the tags listed will be returned. You can exclude
#'  results that match a term by prepending it with a - character.
#'@param tags_any Logical, optional. If TRUE, photos containing any of the tags
#'  will be returned. If FALSE, only photos containing all tags will be
#'  returned. Defaulted to return any tags.
#'@param bbox String, optional bounding box of search area provide as:
#'  "minimum_longitude,minimum_latitude,maximum_longitude,maximum_latitude".
#'@param woe_id String, optional "where on earth identifier" can be supplied
#'  instead of bbox. Use \code{\link{find_place}} to obtain woe_id for a place.
#'@param sf_layer A simple features layer, optional, area to search for photos,
#'  can be supplied instead of a bbox or woeID.
#'@param has_geo Logical, optional parameter for whether returned photos need
#'  associated spatial data.
#'@param mindate_uploaded Character or date, optional. Minimum upload date.
#'  Photos with an upload date greater than or equal to this value will be
#'  returned. The date can be in the form of a unix timestamp or mysql datetime.
#'@param maxdate_uploaded Character or date, optional. Maximum upload date.
#'  Photos with an upload date less than or equal to this value will be
#'  returned. The date can be in the form of a unix timestamp or mysql datetime.
#'@param user_id Character, optional. The Flickr ID of the user who's photo to
#'  search. If this parameter isn't passed then everybody's public photos will
#'  be searched.
#'
#'@return data.frame. Output consists of 57 variables including; latitude and
#'  longitude of photograph, date and time it was taken, associated tags and
#'  image urls.
#'
#'  Full list of variables returned:
#'
#'  \itemize{ \item id: photograph's unique id number \item owner: the unique id
#'  of the Flickr user \item secret: photograph unique secret number \item
#'  server: Flickr server data \item farm: Flickr server data \item title:
#'  photograph title \item ispublic: whether photograph is public; 1 = yes, 0 =
#'  no \item isfriend whether user is friend; 1 = yes, 0 = no \item isfamily
#'  whether user is family; 1 = yes, 0 = no \item license: use licence of the
#'  image see
#'  \url{https://www.flickr.com/services/api/flickr.photos.licenses.getInfo.html}
#'   for details \item datetaken: date and time of image capture \item
#'  datetakengranularity: accuracy of image date see
#'  \url{https://www.flickr.com/services/api/misc.dates.html} for more
#'  information on dates \item datetakenunknown: whether date is unknown see
#'  \url{https://www.flickr.com/services/api/misc.dates.html} for more
#'  information on dates \item count_views: number of view the photograph has
#'  had, \item count_comments: number of comments on the photograph \item
#'  count_faves: number of times the photograph has been favourited \item tags:
#'  user defined tags on the photograph \item latitude: latitude of where the
#'  image was taken \item longitude: longitude of where the image was taken
#'  \item accuracy: accuracy of spatial reference see
#'  \url{https://www.flickr.com/services/api/flickr.photos.search.html } for
#'  more information \item context: a numeric value representing the photo's
#'  geotagginess beyond latitude and longitude
#'  \url{https://www.flickr.com/services/api/flickr.photos.search.html } for
#'  more information \item place_id: unique numeric number representing the
#'  location of the photograph \item woeid: unique numeric number representing
#'  the location of the photograph \item geo_is_family: whether only friends can
#'  see geo; 1 = yes, 0 = no \item geo_is_friend: whether only family can see
#'  geo; 1 = yes, 0 = no \item geo_is_contact: whether only contact can see geo;
#'  1 = yes, 0 = no \item geo_is_public whether geo is public; 1 = yes, 0 = no
#'  \item url_sq: URL for square image \item height_sq: height for square image
#'  \item width_sq: width for square image \item url_t: URL for square image
#'  thumbnail image 100 on longest side \item height_t: height for thumbnail
#'  image 100 on longest side, \item width_t: width for thumbnail image 100 on
#'  longest side \item url_s: URL for small square image 75x75 \item height_s:
#'  height for small square image 75x75 \item width_s: width for small square
#'  image 75x75 \item url_q: URL for large square image 150x150 \item height_q:
#'  height for large square image 150x150 \item width_q: width for large square
#'  image 150x150 \item url_m: URL for small image 240 on longest side \item
#'  height_m: height for small image 240 on longest side \item width_m: width
#'  for small image 240 on longest side \item url_n: URL for small image 320 on
#'  longest side \item height_n: height for small image 320 on longest side
#'  \item width_n: width for small image 320 on longest side \item url_z: URL
#'  for medium image 640 on longest side \item height_z: height for medium image
#'  640 on longest side \item width_z: width for medium image 640 on longest
#'  side \item url_c: URL for medium image 800 on longest side \item height_c:
#'  height for medium image 800 on longest side \item width_c: width for medium
#'  image 800 on longest side \item url_l: URL for large image 1024 on longest
#'  side \item height_l: height for large image 1024 on longest side \item
#'  width_l: width for large image 1024 on longest side \item url_o: URL for
#'  original image, either a jpg, gif or png, depending on source format \item
#'  height_o: height for original image, either a jpg, gif or png, depending on
#'  source format \item width_o: width for original image, either a jpg, gif or
#'  png, depending on source format }
#'
#'@family Search for images
#'
#'@export
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
#'
#' photo_search(
#'   mindate_taken = "2018-01-01",
#'   maxdate_taken = "2018-01-03",
#'   tags = c("mountain", "lake"),
#'   tags_any = TRUE
#' )
#'
#' photo_search(
#'   mindate_taken = "2018-01-01",
#'   maxdate_taken = "2018-01-03",
#'   tags = c("mountain", "lake"),
#'   tags_any = FALSE
#' )
#'
#' }
photo_search <-
  function(mindate_taken = NULL,
           maxdate_taken = NULL,
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

    pics <- NULL
    num_calls <- 0


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

      #flickr seems to search 8 hours in advance of
      mindate_taken <- mindate_taken - 28800

      mindate_unix <- as.numeric(mindate_taken)
      maxdate_unix <- as.numeric(maxdate_taken)

      if (mindate_taken > maxdate_taken){

        date_df <- date_df[-1, ]

      } else {

        base_url <- get_url(
          mindate_taken = mindate_unix,
          maxdate_taken = maxdate_unix,
          mindate_uploaded = mindate_uploaded,
          maxdate_uploaded = maxdate_uploaded,
          user_id = user_id,
          api_key = api_key,
          page = 1,
          text = text,
          tags = tags,
          tag_mode = tags_any,
          bbox = bbox,
          woe_id = woe_id,
          has_geo = has_geo
        )

        photo_xml <- search_url(base_url = base_url)

        #add to number of needed calls
        num_calls <- num_calls + 1

        find_errors(error_xml = photo_xml)

        if (!is.null(photo_xml)) {
          pages_data <- data.frame(
            xml2::xml_attrs(xml2::xml_children(photo_xml)))
          pages_data[] <- lapply(
            pages_data, FUN = function(x) as.integer(as.character(x)))
          total_pages <- pages_data["pages", ]
          total <- pages_data["total", ]

          if (total > 0 && total > 4000){

            for (i in 1:16){

              base_url <- get_url(
                mindate_taken = mindate_unix,
                maxdate_taken = maxdate_unix,
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

                #add to number of needed calls
                num_calls <- num_calls + 1

                if (!is.null(photo_xml)) {
                  photo_atts <- xml2::xml_find_all(
                    photo_xml, "//photo", ns = xml2::xml_ns(photo_xml))
                  tmp_df <- dplyr::bind_rows(lapply(
                    xml2::xml_attrs(photo_atts), function(x) data.frame(
                      as.list(x), stringsAsFactors = FALSE)))

                  pics <- dplyr::bind_rows(pics, tmp_df)

                  tmp_df <- NULL
                }

            }

            # create dataframe with minmaxdate_takens
            date_df <- rbind(
              date_df[-1, ],
              data.frame(mindate_taken = max(pics$datetaken),
                         maxdate_taken = maxdate_taken))

          } else if (total > 0 && total < 4000){

            for (i in 1:total_pages){

              base_url <- get_url(
                mindate_taken = mindate_unix,
                maxdate_taken = maxdate_unix,
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

              #add to number of needed calls
              num_calls <- num_calls + 1

              if (!is.null(photo_xml)) {
                photo_atts <- xml2::xml_find_all(
                  photo_xml, "//photo", ns = xml2::xml_ns(photo_xml))
                tmp_df <- dplyr::bind_rows(lapply(
                  xml2::xml_attrs(photo_atts), function(x) data.frame(
                    as.list(x), stringsAsFactors = FALSE)))

                pics <- dplyr::bind_rows(pics, tmp_df)

                tmp_df <- NULL
              }

            }

            # create dataframe with minmaxdate_takens
            date_df <- date_df[-1, ]

          } else {

            date_df <- date_df[-1, ]

          }


        } else {

          date_df <- date_df[-1, ]

        }

      }

    }

    if (is.null(pics)){

      stop("No photographs meeting criteria")

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


      pics$within <- sf::st_intersects(pics, sf_layer)
      pics$within <- as.character(pics$within)
      pics <- dplyr::filter(pics, pics$within != "integer(0)")

    }

    pics <- parse_pic(pics = pics)

    pics <- dplyr::distinct(pics)

    # end
    return(pics)
  }
