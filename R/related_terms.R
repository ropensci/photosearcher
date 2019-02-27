#relatedtags
related_terms <- 
  function(term = NULL){
    
    if( is.null(term)==TRUE) {
      stop('provide a term')
    }
    
    baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.tags.getRelated&api_key=",api_key,"&tag=", term,sep="")   
    r <- GET(baseURL)
    
    count_stat <- 0
    
    while(r$status_code != 200 & count_stat < 3){
      Sys.sleep(0.5)
      r <- GET(paste(base_url,
                     "&min_taken_date=", as.character(mindate),
                     "&max_taken_date=", as.character(maxdate), sep=""))
      count_stat <-  count_stat + 1
    }
    
    if(r$status_code != 200){
      warning('Status code:', r$status, ' for ', text, ' - message: ', content(r, 'text'))
    }
    
    error <- tryCatch({
      getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
      error <- 'sucess'
    }, error = function(err){
      warning(text, ' skipped beacuse: ', err)
      error <- 'error'
    })    
    
    if(error != 'error'){
      
    getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
    getPhotos_data <<- getPhotos_data
    tag <- listNulltoNA(xpathSApply(doc = getPhotos_data, "//tag", function(n) xmlValue(n[[1]])))
    
    }
    
    return(tag)
    
  }


                    