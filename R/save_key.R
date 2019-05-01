#' save_key
#'
#' Enter a valid Flickr API key.API keys are avialable from
#' \url{https://www.flickr.com/services/apps/create/apply}. The API key will
#' then be saved as a .Rda file and be called to when using any other function.
#'
#' @param api_key String, a valid API key avialable from
#' \url{https://www.flickr.com/services/apps/create/apply}
#'
#' @return Saves you API key as a .rda file so that it can be used for all
#'   functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' save_key <- function(api_key = "your api key") {
#' #' }
#'
save_key <- function(api_key = NULL) {

  if (is.null(api_key)) {
    stop("Provide a valid API key: create an API key at https://www.flickr.com/services/apps/create/apply\n")
  }

  else {
    #check for vailid key
    check_key(key = api_key)

    save(api_key, file = "api_key.Rda")

    load("~/photosearcher/api_key.Rda")

  }

  return(api_key)
}
