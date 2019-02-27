################ Null to NA ###########

listNulltoNA <- function(x){
  if(length(x) == 0){
    return(NA)
  } else {
    x[sapply(x, is.null)] <- NA
    return(x)
  }
}

###

############# Next util ############


###