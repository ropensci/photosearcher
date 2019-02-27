get_licenses <-
  function(){
    
    baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.licenses.getInfo&api_key=",api_key,sep="")   #set base URL
    
    getLicense_data <- getPhotos_data <- xmlRoot(
                                          xmlTreeParse(
                                            getURL(baseURL,
                                                   ssl.verifypeer = FALSE,
                                                   useragent = "flickr")
                                            ,useInternalNodes = TRUE))
    
    id <- xpathSApply(getLicense_data, "//license", xmlGetAttr, "id")                #extract license id
    name <- xpathSApply(getLicense_data, "//license", xmlGetAttr, "name")            #extract license name
    
    tmp_df <- data.frame(id, name, stringsAsFactors = FALSE)
    
    return(tmp_df)
  }