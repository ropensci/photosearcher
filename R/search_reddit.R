reddit_search <- function(search_term = NULL,
                          subreddit = NULL,
                          start_date = "2020-01-01",
                          end_date = "2021-01-01") {

  #combine search terms if multiple
  if(length(search_term) > 1){
    search_term <-  paste(search_term, collapse='+')
  }

  #create base url with dates and search terms
  base_url <- paste("https://api.pushshift.io/reddit/submission/search/",
                    "?&after=",
                    start_date,
                    "&before=",
                    end_date,
                    "&sort=asc",
                    "&limit=100",
                    ifelse(!(is.null(subreddit)), paste0(
                      "&subreddit=", subreddit), ""),
                    ifelse(!(is.null(search_term)), paste0(
                      "&q=", search_term), ""),
                    sep = "")

  #parse api data
  jsondata <- jsonlite::fromJSON(base_url, flatten = TRUE)
  pushshift <- data.table::rbindlist(jsondata, fill = FALSE)

  if(nrow(pushshift) < 1){

    stop("No posts matching your search")

  }

  #find new date to search from
  new_date <- pushshift$created_utc[nrow(pushshift)]
  new_date <- as.POSIXct(new_date, origin= '1970-01-01') + (60 * 60 * 5) #skips 5 hours to stop it getting caught in a loop
  new_date <- gsub(" ", "%20", new_date, fixed = TRUE) #%20 is just space for search

  #check if new search is needed
  check <- nrow(pushshift)

  while(check > 0) {

    #create base url with dates and search terms
    base_url <- paste("https://api.pushshift.io/reddit/submission/search/",
                      "?&after=",
                      new_date,
                      "&before=",
                      end_date,
                      "&sort=asc",
                      "&limit=100",
                      ifelse(!(is.null(subreddit)), paste0(
                        "&subreddit=", subreddit), ""),
                      ifelse(!(is.null(search_term)), paste0(
                        "&q=", search_term), ""),
                      sep = "")

    #parse api data
    jsondata <- jsonlite::fromJSON(base_url, flatten = TRUE)
    tmp_pushshift <- data.table::rbindlist(jsondata, fill = FALSE)

    #merge to all data
    pushshift <- dplyr::bind_rows(pushshift, tmp_pushshift)

    #find new date to search from
    new_date <- pushshift$created_utc[nrow(pushshift)]
    new_date <- as.POSIXct(new_date, origin= '1970-01-01') + (60 * 60 * 5) #to stop it geting caught in a loop
    new_date <- gsub(" ", "%20", new_date, fixed = TRUE) #%20 is just space for search

    #check if new search is needed
    check <- nrow(tmp_pushshift)

  }

  #which columns to keep
  keeps <- c("author",
             "created_utc",
             "title",
             "selftext",
             "url",
             "permalink",
             "score",
             "num_comments",
             "subreddit",
             "over_18")

  #select only the keep cols
  pushshift <- pushshift %>%
    select(all_of(keeps))

  #function to parse api results
  set_lists_to_chars <- function(x) {
    if(class(x) == 'list') {
      y <- paste(unlist(x[1]), sep='', collapse=', ')
    } else {
      y <- x
    }
    return(y)
  }


  #turn results into a df
  pushshift <- data.frame(lapply(pushshift, set_lists_to_chars),
                            stringsAsFactors = F)

  #return data
  return(pushshift)

}
