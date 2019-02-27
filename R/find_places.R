find_places <- function(place){

newURSltester <- paste("https://api.flickr.com/services/rest/?method=flickr.places.find&api_key=", api_key, sep = "") 
query <- place

getPhotos <- paste0(newURSltester
                    ,"&query=",query)

getPhotos_data <- xmlRoot(xmlTreeParse(getURL
                                       (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr")
                                       ,useInternalNodes = TRUE ))

place_id<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"place_id")                 
woe_id<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"woeid")                 
latitude<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"latitude")                 
longitude<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"longitude")                 
place_url<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"place_url")                 
woe_name<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"woe_name")                 

places<-data.frame(cbind(woe_name,place_url,place_id,woe_id,latitude,longitude))

return(places)
}
