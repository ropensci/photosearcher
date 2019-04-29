#load required packages
devtools::load_all(".")
library(dplyr)
library(sf)
library(ggplot2)

#search for photos
x <- photo_search(mindate = "2018-01-01", maxdate = "2019-01-01", text = "adder", bbox = "-11.425781,49.468124,2.900391,60.196156")

#turn lat/long from dataframe into points
photos_points <- x %>%
  rename(long = longitude,
         lat = latitude) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#uk map
UK <- map_data(map = "world", region = "UK")

#plot map
ggplot() +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  geom_sf(data = photos_points, color = "red")


#download images
download_images(photo_id = x$id[1-10], saveDir = "demonstration")

#get user info
y <- user_info(user_id = x$owner[3])


