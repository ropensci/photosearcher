#' Search Reddit for posts
#'
#' Searches for a a given term or set of terms across Reddit between chosen dates.
#' Can be limited to a specific subreddit.
#'
#' @param search_term
#' @param subreddit
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
reddit_search <- function(search_term = NULL,
                          subreddit = NULL,
                          start_date = "2020-01-01",
                          end_date = "2021-01-01") {

  #combine search terms if multiple
  if(length(search_term) > 1){
    search_term <-  paste(search_term, collapse='+')
  }

  #search is in EST not GMT so need to plus 5 hours
  start_date <- as.POSIXct(start_date, origin = '1970-01-01') + (60 * 60 * 5)
  end_date <- as.POSIXct(end_date, origin = '1970-01-01') + (60 * 60 * 5)

  #initiate progress bar
  pb = utils::txtProgressBar(min = 0, max = as.numeric(round(difftime(end_date,
                                                               start_date,
                                                               units = "days"))),
                      initial = 0)


  #Fix spaces in
  start_date <- gsub(" ", "%20", start_date, fixed = TRUE)
  end_date <- gsub(" ", "%20", end_date, fixed = TRUE)
  search_term <- gsub(" ", "%20", search_term, fixed = TRUE)


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

  new_date <- as.POSIXct(new_date, origin= '1970-01-01')
  + (60 * 60 * 5) #again add 5 hours to stop it getting caught in a loop

  #new tick for progress bar
  new_tick <- as.numeric(round(difftime(new_date,
                                        start_date,
                                        units = "days")))

  #update progress bar
  utils::setTxtProgressBar(pb, new_tick)

  #%20 is just a space that is needed for the api search
  new_date <- gsub(" ", "%20", new_date, fixed = TRUE)


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

    #save old date for progress bar
    old_date <- new_date

    #find new date to search from
    new_date <- pushshift$created_utc[nrow(pushshift)]
    new_date <- as.POSIXct(new_date, origin= '1970-01-01') +
      (60 * 60 * 5) #to stop it getting caught in a loop

    #new tick for progress bar
    new_tick <- as.numeric(round(difftime(new_date,
                                          start_date,
                                          units = "days")))

    #update progress bar
    utils::setTxtProgressBar(pb, new_tick)

    #%20 is just space for search
    new_date <- gsub(" ", "%20", new_date, fixed = TRUE)

    #current check: to be deleted
    #print(new_date)

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
    dplyr::select(all_of(keeps))

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
  pushshift <- data.frame(lapply(pushshift,
                                 set_lists_to_chars),
                            stringsAsFactors = F)

  pushshift$date_created <- as.POSIXct(pushshift$created_utc,
                                       origin= '1970-01-01')

  close(pb)

  #return data
  return(pushshift)

}
