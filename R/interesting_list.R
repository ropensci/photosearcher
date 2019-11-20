#' Interesting list
#'
#' Returns a Flickr generated list of photographs deemed interesting. For
#' information on how this list is calculated see:
#' \url{http://www.steves-digicams.com/knowledge-center/how-tos/online-sharing-social-networking/what-is-flickr-interestingness.html#b}
#'
#'When running the function you need an API key saved as
#'photosearcher_key.sysdata in your working directory. If this is the first
#'function you run you will be prompted to create and enter your API key. The
#'API key will then be saved as photosearcher_key.sysdata in your working
#'directory and is used for all function.
#'
#' @param date Character, required. Interestingness list for the date provided.
#'   The date should be in the form of "YYYY-MM-DD".
#'
#' @return data.frame. Output consists of 57 variables including; latitude and
#'   longitude of photograph, date and time it was taken, associated tags and
#'   image urls.
#'
#' Full list of variables returned:
#'
#' \itemize{
#'   \item id: photograph's unique id number
#'   \item owner: the unique id of the Flickr user
#'   \item secret: photograph unique secret number
#'   \item server: Flickr server data
#'   \item farm: Flickr server data
#'   \item title: photograph title
#'   \item ispublic: whether photograph is public; 1 = yes, 0 = no
#'   \item isfriend whether user is friend; 1 = yes, 0 = no
#'   \item isfamily whether user is family; 1 = yes, 0 = no
#'   \item license: use licence of the image see \url{https://www.flickr.com/services/api/flickr.photos.licenses.getInfo.html} for details
#'   \item datetaken: date and time of image capture
#'   \item datetakengranularity: accuracy of image date see \url{https://www.flickr.com/services/api/misc.dates.html} for more information on dates
#'   \item datetakenunknown: whether date is unknown see \url{https://www.flickr.com/services/api/misc.dates.html} for more information on dates
#'   \item count_views: number of view the photograph has had,
#'   \item count_comments: number of comments on the photograph
#'   \item count_faves: number of times the photograph has been favourited
#'   \item tags: user defined tags on the photograph
#'   \item latitude: latitude of where the image was taken
#'   \item longitude: longitude of where the image was taken
#'   \item accuracy: accuracy of spatial reference see \url{https://www.flickr.com/services/api/flickr.photos.search.html } for more information
#'   \item context: a numeric value representing the photo's geotagginess beyond latitude and longitude \url{https://www.flickr.com/services/api/flickr.photos.search.html } for more information
#'   \item place_id: unique numeric number representing the location of the photograph
#'   \item woeid: unique numeric number representing the location of the photograph
#'   \item geo_is_family: whether only friends can see geo; 1 = yes, 0 = no
#'   \item geo_is_friend: whether only family can see geo; 1 = yes, 0 = no
#'   \item geo_is_contact: whether only contact can see geo; 1 = yes, 0 = no
#'   \item geo_is_public whether geo is public; 1 = yes, 0 = no
#'   \item url_sq: URL for square image
#'   \item height_sq: height for square image
#'   \item width_sq: width for square image
#'   \item url_t: URL for square image thumbnail image 100 on longest side
#'   \item height_t: height for thumbnail image 100 on longest side,
#'   \item width_t: width for thumbnail image 100 on longest side
#'   \item url_s: URL for small square image 75x75
#'   \item height_s: height for small square image 75x75
#'   \item width_s: width for small square image 75x75
#'   \item url_q: URL for large square image 150x150
#'   \item height_q: height for large square image 150x150
#'   \item width_q: width for large square image 150x150
#'   \item url_m: URL for small image 240 on longest side
#'   \item height_m: height for small image 240 on longest side
#'   \item width_m: width for small image 240 on longest side
#'   \item url_n: URL for small image 320 on longest side
#'   \item height_n: height for small image 320 on longest side
#'   \item width_n: width for small image 320 on longest side
#'   \item url_z: URL for medium image 640 on longest side
#'   \item height_z: height for medium image 640 on longest side
#'   \item width_z: width for medium image 640 on longest side
#'   \item url_c: URL for medium image 800 on longest side
#'   \item height_c: height for medium image 800 on longest side
#'   \item width_c: width for medium image 800 on longest side
#'   \item url_l: URL for large image 1024 on longest side
#'   \item height_l: height for large image 1024 on longest side
#'   \item width_l: width for large image 1024 on longest side
#'   \item url_o: URL for original image, either a jpg, gif or png, depending on source format
#'   \item height_o: height for original image, either a jpg, gif or png, depending on source format
#'   \item width_o: width for original image, either a jpg, gif or png, depending on source format
#'}
#'
#' @family Search for images
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' interesting_list(date = "2019-01-01")
#'
#' interesting_list(date = "2017-05-05")
#'
#' interesting_list(date = "2011-11-25")
#'
#' }
interesting_list <-
  function(date = "2019-01-01"){

    # this checks for the presence of a key, if no key it prompts the user to
    # create one, it then checks the validity of the key
    api_key <- create_and_check_key()

    pics <- NULL

    base_url <- paste("https://api.flickr.com/services/rest/",
                      "?method=flickr.interestingness.getList&api_key=",
                      api_key,
                      "&date=",
                      date,
                      "&extras=",
                      "&extras=", "description,date_taken,geo,tags,license,",
                      "url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,",
                      "url_l,url_o,count_views,count_comments,count_faves",
                      "&per_page=500",
                      sep = "")

        photo_xml <- search_url(base_url = base_url)

    find_errors(error_xml = photo_xml)

    if (!is.null(photo_xml)) {
      photo_atts <- xml2::xml_find_all(
        photo_xml, "//photo", ns = xml2::xml_ns(photo_xml))
      pics <- dplyr::bind_rows(lapply(
        xml2::xml_attrs(photo_atts), function(x) data.frame(
          as.list(x), stringsAsFactors = FALSE)))

    }

    return(pics)

  }
