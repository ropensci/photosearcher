#' Interesting list
#'
#' Returns a Flickr generated list of photographs deemed interesting. For
#' information on how this list is calculated see:
#' \link{http://www.steves-digicams.com/knowledge-center/how-tos/online-sharing-social-networking/what-is-flickr-interestingness.html#b}
#'
#' @param date Character, required. Interestingness list for the date provided.
#'   The date should be in the form of "YYYY-MM-DD".
#'
#' @returndata.frame. Output consists of 57 variables including; latitude and
#' longitude of photograph, date and time it was taken, associated tags and
#' image urls.
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
