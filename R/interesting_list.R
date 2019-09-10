#' interesting_list
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
#' Full list of variables returned: id, owner, secret, server, farm, title,
#' ispublic, isfriend, isfamily, license, datetaken, datetakengranularity,
#' datetakenunknown, count_views, count_comments, count_faves, tags, latitude,
#' longitude, accuracy, context, place_id, woeid, geo_is_family, geo_is_friend,
#' geo_is_contact, geo_is_public, url_sq, height_sq, width_sq, url_t, height_t,
#' width_t, url_s, height_s,	width_s	url_q, height_q, width_q, url_m, height_m,
#' width_m, url_n, height_n, width_n, url_z, height_z, width_z, url_c, height_c,
#' width_c, url_l, height_l, width_l, url_o, height_o, width_o.
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
