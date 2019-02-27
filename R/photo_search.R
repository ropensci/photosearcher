photo_search <-
  function(min_taken = "2018-01-01",
           max_taken = "2019-01-01",
           text = NULL,          
           bbox = NULL,
           woe_id = NULL,     
           has_geo = TRUE){
    
    if( !(is.null(bbox) | is.null(woe_id))==TRUE) {
      
      stop('can not provide bbox and woe_id')
    }
    
    text <- gsub(' ', '+', trimws(text))  
    mindate <- min_taken
    maxdate <- max_taken
    pics <- NULL
    spatial_df <- NULL
    
    # set the base search url
    base_url <- if(!is.null(bbox)){
      paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,
                          "&text=", text,
                          "&bbox=", paste(bbox),
                          "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o",
                          "&per_page=", "250",
                          "&format=", "rest",
                          sep = "")
    } else if(!is.null(woe_id)){
      paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,
                          "&text=", text,
                          "&woe_id=", woe_id,
                          "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o",
                          "&per_page=", "250",
                          "&format=", "rest",
                          sep = "")     
    } else {
      paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,
                          "&text=", text,
                          ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                          "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o",
                          "&per_page=", "250",
                          "&format=", "rest",
                          sep = "")
    
    }
    
    # create dfs so large searches can be subset dynamically 
    date_df <- data.frame(mindate = mindate, maxdate = maxdate)
    
    # start while loop - until all dates are looped through
    while(nrow(date_df) > 0) {
      
      # set search dates
      mindate = date_df[1, "mindate"]
      maxdate = date_df[1, "maxdate"]
      
      # get total number of results
      r <- GET(paste(base_url,
                     "&min_taken_date=", as.character(mindate),
                     "&max_taken_date=", as.character(maxdate), sep=""))
      
      #put first error catch here
      count_stat <- 0
      
      while(r$status_code != 200 & count_stat < 3){
        Sys.sleep(0.5)
        r <- GET(paste(base_url,
                       "&min_taken_date=", as.character(mindate),
                       "&max_taken_date=", as.character(maxdate), sep=""))
        count_stat <-  count_stat + 1
      }
      
      if(r$status_code != 200){
        warning('Status code:', r$status, ' for ', mindate, ' to ', maxdate, ' - message: ', content(r, 'text'))
      }
      
      error <- tryCatch({
        getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
        error <- 'sucess'
      }, error = function(err){
        warning('Dates between ', mindate, ' and ', maxdate, ' skipped beacuse: ', err)
        error <- 'error'
      })    
      
      if(error != 'error'){
      
      getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
      pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
      pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
      total_pages <- pages_data["pages",]
      total <- pages_data["total",]
      
      #if > 4000 and not single days, split
      if(total > 4000 && (as.Date(mindate) != (as.Date(maxdate) - 1))) {
        
        x <- ceiling(total/4000)
        y <- length(seq(as.Date(mindate), as.Date(maxdate), by="+1 day"))
        
        #if x + 1 is larger than number of days, split in to single days
        if (x + 1 > y){
          
          dates <- seq(as.Date(mindate), as.Date(maxdate), by="+1 day")
          
        }
        
        #else split accoring to x
        else {
          
          dates <- seq(as.Date(mindate), as.Date(maxdate), length.out = x + 1)
          
        }
        
        # create dataframe with minmaxdates
        date_df <- rbind(date_df[-1,], data.frame(mindate = dates[1:(length(dates) - 1)], maxdate = dates[2:length(dates)]))
      
        }
      
      #if > 4000 and single days, pass days to be split by area
      else if (total > 4000 && (as.Date(mindate) == (as.Date(maxdate) - 1))){
        
        spatial_df <- rbind(spatial_df, data.frame(mindate = mindate, maxdate = maxdate))
        
        date_df <- date_df[-1,]
        
      }
      
      #if all conditions are satisfied get data
      else if (total <= 4000 && total_pages > 0){ 
        
        #get data second error catch here
        pics_tmp <- NULL
        
        # loop thru pages of photos and save the list in a DF
        for(i in c(1:total_pages)){
          
          
          r <- GET(paste(base_url,
                         "&min_taken_date=", as.character(mindate),
                         "&max_taken_date=", as.character(maxdate), "&page="
                         ,i, sep=""))
          
          count_stat <- 0
          
          while(r$status_code != 200 & count_stat < 3){
            Sys.sleep(0.5)
            r <- GET(paste(base_url,
                           "&min_taken_date=", as.character(mindate),
                           "&max_taken_date=", as.character(maxdate), "&page="
                           ,i, sep=""))
            count_stat <-  count_stat + 1
          }
          
          if(r$status_code != 200){
            warning('Status code:', r$status, ' for dates between ', mindate, ' and ', maxdate, ' page ', i, ' - message: ', content(r, 'text'))
          }
          
          error <- tryCatch({
            getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
            error <- 'sucess'
          }, error = function(err){
            warning('Dates between ', mindate, ' and ', maxdate, ' page ', i,' skipped beacuse: ', err)
            error <- 'error'
          })
          
          if(error != 'error'){
            getPhotos_data <<- getPhotos_data
            id <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "id"))                 #extract photo id
            owner <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "owner"))           #extract user id
            datetaken <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "datetaken"))   #extract date picture was taken
            tags <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "tags"))             #extract tags
            latitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "latitude"))     #extract latitude
            longitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "longitude"))   #extract longitude
            license <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "license"))       #extract license
            url_s <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_s"))           #extract url_s
            url_m <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_m"))           #extract url_m
            url_l <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_l"))           #extract url_l
            url_o <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_o"))           #extract url_o
            
            if(!all(is.na(c(id,owner,datetaken,tags,latitude,longitude,license,url_s,url_m,url_l,url_o)))){
              
              tmp_df <- data.frame(id, owner, datetaken, tags,
                                   as.numeric(latitude),
                                   as.numeric(longitude), license,
                                   url_s = unlist(url_s), url_m = unlist(url_m),
                                   url_l = unlist(url_l), url_o = unlist(url_o),
                                   stringsAsFactors = FALSE)
              
              tmp_df$page <- i
              pics_tmp <- rbind(pics_tmp, tmp_df)
              rm(list = 'tmp_df')
              
            }
          }
        }
        
        pics <- rbind(pics, pics_tmp)
        
        date_df <- date_df[-1,]
        
        #end second error catch here
        
        }

      #else search is empty, skip
      else {
        
        date_df <- date_df[-1,]
        
      } 
      
      #end first error catch here
      }
      
    }
    
    #split single days by area if bbox is available 
    if (!is.null(bbox) && !is.null(spatial_df)){
      
      #split by dates
      strbbox <- unlist(strsplit(bbox, ","), use.names = FALSE)
      spatial_df$xmin <- strbbox[1]
      spatial_df$ymin <- strbbox[2]
      spatial_df$xmax <- strbbox[3]
      spatial_df$ymax <- strbbox[4]
      
      base_url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,
              "&text=", text,
              "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o",
              "&per_page=", "250",
              "&format=", "rest",
              sep = "")
      
      while(nrow(spatial_df) > 0) {
        mindate = spatial_df[1, "mindate"]
        maxdate = spatial_df[1, "maxdate"]
        xmin = spatial_df[1, "xmin"]
        ymin = spatial_df[1, "ymin"]
        xmax = spatial_df[1, "xmax"]
        ymax = spatial_df[1, "ymax"]
        
        bbox <- paste(xmin, ",", ymin, ",", xmax, ",", ymax, sep = "")
        
        #then build search_eq
        r <- GET(paste(base_url,
                       "&min_taken_date=", as.character(mindate),
                       "&max_taken_date=", as.character(maxdate), "&bbox=", paste(bbox), sep=""))
        
        #put first error catch here
        count_stat <- 0
        
        while(r$status_code != 200 & count_stat < 3){
          Sys.sleep(0.5)
          r <- GET(paste(base_url,
                         "&min_taken_date=", as.character(mindate),
                         "&max_taken_date=", as.character(maxdate), "&bbox=", bbox, sep=""))
          
          count_stat <-  count_stat + 1
        }
        
        if(r$status_code != 200){
          warning('Status code:', r$status, ' for bbox ', bbox, ' date ', mindate, 'to', maxdate, ' - message: ', content(r, 'text'))
        }
        
        error <- tryCatch({
          getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
          error <- 'sucess'
        }, error = function(err){
          warning('Year ', y, ' month ', m, ' skipped beacuse: ', err)
          error <- 'error'
        })    
        
        if(error != 'error'){
          
          getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
          pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
          pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
          total_pages <- pages_data["pages",]
          total <- pages_data["total",]
          
          print(total)
          
          #if less than 4000 and greater than 0
          if (total <= 4000 && total > 0){
            
            #get data second error catch here
            pics_tmp <- NULL
            
            # loop thru pages of photos and save the list in a DF
            for(i in c(1:total_pages)){
              
              
              r <- GET(paste(base_url,
                             "&min_taken_date=", as.character(mindate),
                             "&max_taken_date=", as.character(maxdate), "&page="
                             ,i, sep=""))
              
              count_stat <- 0
              
              while(r$status_code != 200 & count_stat < 3){
                Sys.sleep(0.5)
                r <- GET(paste(base_url,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate), "&page="
                               ,i, sep=""))
                count_stat <-  count_stat + 1
              }
              
              if(r$status_code != 200){
                warning('Status code:', r$status, ' for dates between ', mindate, ' and ', maxdate, ' page ', i, ' - message: ', content(r, 'text'))
              }
              
              error <- tryCatch({
                getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
                error <- 'sucess'
              }, error = function(err){
                warning('Dates between ', mindate, ' and ', maxdate, ' page ', i,' skipped beacuse: ', err)
                error <- 'error'
              })
              
              if(error != 'error'){
                getPhotos_data <<- getPhotos_data
                id <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "id"))                 #extract photo id
                owner <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "owner"))           #extract user id
                datetaken <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "datetaken"))   #extract date picture was taken
                tags <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "tags"))             #extract tags
                latitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "latitude"))     #extract latitude
                longitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "longitude"))   #extract longitude
                license <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "license"))       #extract license
                url_s <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_s"))           #extract url_s
                url_m <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_m"))           #extract url_m
                url_l <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_l"))           #extract url_l
                url_o <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_o"))           #extract url_o
                
                if(!all(is.na(c(id,owner,datetaken,tags,latitude,longitude,license,url_s,url_m,url_l,url_o)))){
                  
                  tmp_df <- data.frame(id, owner, datetaken, tags,
                                       as.numeric(latitude),
                                       as.numeric(longitude), license,
                                       url_s = unlist(url_s), url_m = unlist(url_m),
                                       url_l = unlist(url_l), url_o = unlist(url_o),
                                       stringsAsFactors = FALSE)
                  
                  tmp_df$page <- i
                  pics_tmp <- rbind(pics_tmp, tmp_df)
                  rm(list = 'tmp_df')
                  
                }
              }
            }
            
            pics <- rbind(pics, pics_tmp)
            
            spatial_df <- rbind(spatial_df[-1,])
            
          }
          
          #if more than 4000 split
          else if (total > 4000 && (xmin != xmax | ymin != ymax)){
            
            #find the right number of squares needed
            num <- ceiling(total/4000)
            num_sqrt <- sqrt(num)
            num_next <- ceiling(num_sqrt)
            sqr_next <- num_next * num_next
            
            #number to sequence x and y coords by
            z <- sqrt(sqr_next) + 1
            
            xseq <- seq(xmin, xmax, length.out = z)
            yseq <- seq(ymin, ymax, length.out = z)
            
            xy_df <- NULL
            
            for (y in 1:(length(yseq) -1)){
              
              x_df <- data.frame(xmin = xseq[1:(length(xseq) - 1)],  xmax = xseq[2:(length(xseq))])
              y_df <- data.frame(ymin = yseq[y], ymax = yseq[y + 1])
              
              z_df <- data.frame(mindate = mindate, maxdate = maxdate, xmin = x_df$xmin, ymin = y_df$ymin, xmax = x_df$xmax, ymax = y_df$ymax)
              xy_df <- rbind(xy_df, z_df)
              
              rm(list = 'z_df')
              
            }
            
            spatial_df <- rbind(spatial_df[-1,], xy_df)
            
          }
          
          #if bbox coords become points 
          else if (total > 4000 && (xmin == xmax && ymin == ymax)){ 
            
            print("big match")
            
            #warn that single point > 4000 photos a day
            warning("Location ", bbox, " between dates", mindate, " and ", maxdate, " skipped as > 4000 returns")
            
            spatial_df <- rbind(spatial_df[-1,])
            
          }
          
          #else if less than 1 skip date 
          else {
            
            print("else")
            
            spatial_df <- rbind(spatial_df[-1,])
            
          }
          
        }
        
      }
      
      
    }
    
    #return results
    return(pics)
    
    }
    


