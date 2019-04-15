#' @noRd

# get/save api key
get_key <- function() {
  if (exists("api_key")) {


  } else {
    if (file.exists("api_key.Rda")) {
      load("~/photosearcher/api_key.Rda")
    } else {
      message("Create an API key at https://www.flickr.com/services/apps/create/apply\n")
      api_key <- readline(prompt = "Enter API key: ")
      save(api_key, file = "api_key.Rda")

      load("~/photosearcher/api_key.Rda")
    }
  }

  return(api_key)
}

# make this get_api_key function put in all other functions

# try making as case_when
