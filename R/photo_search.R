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
#'@param bbox String, optional bounding box of search area provide as:
#'  "minimum_longitude,minimum_latitude,maximum_longitude,maximum_latitude".
#'@param sf_layer A simple features layer, optional, area to search for photos,
#'  can be supplied instead of a bbox or woeID.
#'@param mindate_uploaded Character or date, optional. Minimum upload date.
#'  Photos with an upload date greater than or equal to this value will be
#'  returned. The date can be in the form of a unix timestamp or mysql datetime.
#'@param maxdate_uploaded Character or date, optional. Maximum upload date.
#'  Photos with an upload date less than or equal to this value will be
#'  returned. The date can be in the form of a unix timestamp or mysql datetime.
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
#'  png, depending on source format \item description: Flickr user generated
#'  text description of the photograph}
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
#'   maxdate_uploaded = "2017-05-05"
#'   )
#'
#' photo_search(
#'   mindate_taken = "2018-01-01",
#'   maxdate_taken = "2018-01-03"
#' )
#'
#' photo_search(
#'   mindate_taken = "2018-01-01",
#'   maxdate_taken = "2018-01-03"
#' )
#'
#' }

photo_search <-
  function(bbox = NULL,
           text = NULL,
           mindate_taken = NULL,
           maxdate_taken = NULL,
           mindate_uploaded = NULL,
           maxdate_uploaded = NULL,
           sf_layer = NULL){


    df <- NULL
    out <- NULL

    # this checks for the presence of a key, if no key it prompts the user to
    # create one, it then checks the validity of the key
    api_key <- create_and_check_key()

    # check that a search location is given
    if ((is.null(bbox) & is.null(sf_layer))) {
      stop("Specify search location as one of: bbox or sf_layer.")
    }

    # check that a search location is given
    if (is.null(text)) {
      stop("Currently boundless searchers are crashing the API, please add a search text")
    }


    # check that only one search location is given
    if ((!is.null(bbox) & !is.null(sf_layer))) {
      stop("Specify search location as only one of: bbox or sf_layer.")
    }


    # change sf_layer to bbox
    if (!is.null(sf_layer)) {

      bbox <- create_bbox(sf_layer = sf_layer)

    }

    base_url <- paste("https://www.flickr.com/services/rest/",
                      "?method=flickr.photos.search&api_key=",api_key,
                      "&bbox=", bbox,
                      ifelse(!(is.null(text)), paste0("&text=", text), ""),
                      ifelse(!(is.null(mindate_taken)), paste0(
                        "&min_taken_date=", mindate_taken), ""),
                      ifelse(!(is.null(maxdate_uploaded)), paste0(
                        "&max_taken_date=", maxdate_taken), ""),
                      ifelse(!(is.null(mindate_uploaded)), paste0(
                        "&min_upload_date=", mindate_uploaded), ""),
                      ifelse(!(is.null(maxdate_uploaded)), paste0(
                        "&max_upload_date=", maxdate_uploaded), ""),
                      "&page=1",
                      "&format=json&nojsoncallback=1",
                      sep= "")

    #print(base_url)

    #parse api data
    jsondata <- jsonlite::fromJSON(base_url, flatten = TRUE)

    #get number of photographs from in the grid
    num_photos <- jsondata[["photos"]][["total"]]

    # check that a search location is given
    if (num_photos == 0) {
      stop("No photographs matching criteria. Note: boundless searches may return as zero")
    }

    df <- data.frame(bbox = bbox,
                     num_photos = num_photos)

    #make highest  grid the top row
    df <- dplyr::arrange(df, by = -num_photos)

    while(max(df$num_photos) > 4000){

      tmp_bbox <- df$bbox[1]

      #get number of photographs from in the grid
      num_photos <- df$num_photos[1]

      #estimate the number of needed bboxs if photos were regular
      bbox_esitmate <- ceiling(num_photos/4000)

      #esitamte number of rows and colums needed in the new grid
      grid_esitmate <- ceiling(bbox_esitmate / 4) + 1

      #split bbox, to build grid
      bbox_split <- stringr::str_split(tmp_bbox, ",", simplify = TRUE)

      #build sf bbox
      g_bbox <- sf::st_bbox(c(xmin = as.numeric(bbox_split[1]),
                              ymin = as.numeric(bbox_split[2]),
                              xmax = as.numeric(bbox_split[3]),
                              ymax = as.numeric(bbox_split[4])),
                            crs = 4326)

      #sf bbox to poly
      g_poly <- sf::st_as_sfc(g_bbox)

      #split grid
      g_grid <- sf::st_make_grid(g_poly, n = c(grid_esitmate, grid_esitmate))

      new_bbox <- data.frame(do.call("rbind",
                                     lapply(g_grid %>% sf::st_transform(4326),
                                            sf::st_bbox)))

      #get values for new bbox and add them
      for(i in 1:nrow(new_bbox)){

        tmp_bbox <- do.call(paste, c(new_bbox[i,], sep=","))

        tmp_url <- paste("https://www.flickr.com/services/rest/",
                         "?method=flickr.photos.search&api_key=",api_key,
                         "&bbox=", tmp_bbox,
                         ifelse(!(is.null(text)), paste0("&text=", text), ""),
                         ifelse(!(is.null(mindate_taken)), paste0(
                           "&min_taken_date=", mindate_taken), ""),
                         ifelse(!(is.null(maxdate_uploaded)), paste0(
                           "&max_taken_date=", maxdate_taken), ""),
                         ifelse(!(is.null(mindate_uploaded)), paste0(
                           "&min_upload_date=", mindate_uploaded), ""),
                         ifelse(!(is.null(maxdate_uploaded)), paste0(
                           "&max_upload_date=", maxdate_uploaded), ""),
                         "&page=1",
                         "&format=json&nojsoncallback=1",
                         sep= "")

        #parse api data
        jsondata <- jsonlite::fromJSON(tmp_url, flatten = TRUE)

        #get number of photographs from in the grid
        num_photos <- jsondata[["photos"]][["total"]]

        tmp_df <- data.frame(bbox = tmp_bbox,
                             num_photos = num_photos)

        df <- dplyr::bind_rows(df, tmp_df)

      }

      #remove top value
      df <- df[-1,]

      #make highest  grid the top row
      df <- dplyr::arrange(df, by = -num_photos)

    }

    #now get the pages for each box
    df <- subset(df, num_photos > 0)

    #initiate progress bar
    pb = utils::txtProgressBar(min = 0,
                               max = nrow(df),
                               initial = 0)

  print(nrow(df))

    for(i in 1:nrow(df)){

      #update progress bar
      utils::setTxtProgressBar(pb, i)

      tmp_bbox <- df$bbox[i]

      tmp_url <- paste("https://www.flickr.com/services/rest/",
                       "?method=flickr.photos.search&api_key=",api_key,
                       "&bbox=", tmp_bbox,
                       ifelse(!(is.null(text)), paste0("&text=", text), ""),
                       ifelse(!(is.null(mindate_taken)), paste0(
                         "&min_taken_date=", mindate_taken), ""),
                       ifelse(!(is.null(maxdate_uploaded)), paste0(
                         "&max_taken_date=", maxdate_taken), ""),
                       ifelse(!(is.null(mindate_uploaded)), paste0(
                         "&min_upload_date=", mindate_uploaded), ""),
                       ifelse(!(is.null(maxdate_uploaded)), paste0(
                         "&max_upload_date=", maxdate_uploaded), ""),
                       "&per_page=500",
                       "&page=1",
                       "&format=json&nojsoncallback=1",
                       sep= ""
      )

      #parse api data
      jsondata <- jsonlite::fromJSON(tmp_url, flatten = TRUE)

      #get number of photographs
      num_photos <- jsondata[["photos"]][["total"]]

      #get number of pages
      num_pages <- jsondata[["photos"]][["pages"]]

      #print(num_pages)

      if (num_photos > 0){

        for (j in 1:num_pages){

          tmp_url <- paste("https://www.flickr.com/services/rest/",
                           "?method=flickr.photos.search&api_key=",api_key,
                           "&bbox=", tmp_bbox,
                           ifelse(!(is.null(text)), paste0("&text=", text), ""),
                           ifelse(!(is.null(mindate_taken)), paste0(
                             "&min_taken_date=", mindate_taken), ""),
                           ifelse(!(is.null(maxdate_uploaded)), paste0(
                             "&max_taken_date=", maxdate_taken), ""),
                           ifelse(!(is.null(mindate_uploaded)), paste0(
                             "&min_upload_date=", mindate_uploaded), ""),
                           ifelse(!(is.null(maxdate_uploaded)), paste0(
                             "&max_upload_date=", maxdate_uploaded), ""),
                           "&extras=", "date_taken,geo,tags,license,",
                           "url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,",
                           "url_l,url_o,count_views,count_comments,count_faves,",
                           "date_upload,last_update,description",
                           "&per_page=500",
                           "&page=", j, #page is now j
                           "&format=json&nojsoncallback=1",
                           sep= "")

          #parse api data
          jsondata <- jsonlite::fromJSON(tmp_url, flatten = TRUE)

          tmp_out <- data.frame(lapply(jsondata[["photos"]][["photo"]],
                                       as.character))

          out <- dplyr::bind_rows(out, tmp_out)

        }

      }

    }

    # is using sf_layer clip results to layer
    if (!is.null(sf_layer)) {
      with_geom <- sf::st_as_sf(out,
                                coords = c("longitude", "latitude"),
                                crs = 4326)

      out <- cbind(with_geom,
                   longitude = out$longitude,
                   latitude = out$latitude)

      sf_layer <- sf::st_transform(
        sf_layer, crs = "+proj=longlat +datum=WGS84 +no_defs")


      out$within <- sf::st_intersects(out, sf_layer)
      out$within <- as.character(out$within)
      out <- dplyr::filter(out, out$within != "integer(0)")

    }

    out <- parse_pic(pics = out)

    out <- dplyr::distinct(out)

    close(pb)

    return(out)

  }
