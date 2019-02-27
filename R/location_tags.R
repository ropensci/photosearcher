location_tags <- function(woe_id = NULL){

  if(is.null(woe_id)==TRUE){
    stop('provide woe_id')
  }

  get_tags <- paste("https://api.flickr.com/services/rest/?method=flickr.places.tagsForPlace&api_key=",api_key,"&woe_id=",woe_id,sep="")

  r <- httr::GET(get_tags)

  count_stat <- 0

  while(r$status_code != 200 & count_stat < 3){
    Sys.sleep(0.5)
    r <- httr::GET(get_info)
    count_stat <-  count_stat + 1
  }

  if(r$status_code != 200){
    warning('Status code:', r$status, ' for location ', woe_id, ' - message: ', httr::content(r, 'text'))
  }

  error <- tryCatch({
    tag_data <- XML::xmlRoot(XML::xmlTreeParse(httr::content(r, 'text'), useInternalNodes = TRUE))
    error <- 'sucess'
  }, error = function(err){
    warning('Locatiion ', woe_id, ' skipped beacuse: ', err)
    error <- 'error'
  })

  if(error != 'error'){
    tag <- XML::xpathSApply(doc = tag_data, "//tag", function(n) XML::xmlValue(n[[1]]))
    count <- listNulltoNA(XML::xpathSApply(tag_data, "//tag", XML::xmlGetAttr, "count"))

    if(!all(is.na(c(tag, count)))){

      location_tags <- data.frame(tag,count)
    }
  }
}





