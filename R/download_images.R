download_images <-
  function(photo_search_results,
           licenses = 7:10,          
           saveDir = '.',
           max_quality = 2,
           verbose = TRUE){
    
    if(!dir.exists(saveDir)){
      message(paste('saveDir', saveDir, 'does not exist, I will create it for you'))
      dir.create(saveDir, recursive = TRUE)
    }
    
    # get those that forefill the license requirements
    toGet <- photo_search_results[photo_search_results$license %in% licenses, ]
    
    if(nrow(toGet) == 0) stop('No images match these license conditions')
      
    # Download the images
    biggest_url <- function(x){
      
      bu <- tail(x = na.omit(x), 1)
      cat(bu)
      if(length(bu) == 0){
        return(NA)
      } else {
        return(bu)
      }
      
    }
    
    quality <- c('url_s', 'url_m', 'url_l', 'url_o')[1:max_quality]
    
    downloadURLs <- apply(toGet[, quality, drop = FALSE], MARGIN = 1, FUN = biggest_url)
    
    downloadURLs[is.na(downloadURLs)] <- toGet[is.na(downloadURLs), 'url_o'] 
    
    dump <- sapply(downloadURLs, all = downloadURLs, verbose = verbose,
                   FUN = function(x, all, verbose){
      if(verbose) cat(paste0('File ',grep(x,all),' of ',length(all),'\n'))
      download.file(url = x,
                    destfile = file.path(saveDir, basename(x)),
                    mode = 'wb')
      })
    
    return(downloadURLs)
    
  }