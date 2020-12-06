
## Set libraries
library(tidyverse)
library(sf)
library(leaflet)

  suppressMessages(imagessf <- sf::st_read(file.path(getwd(), 'trips', 'channel_preserve', 'data', 'images.shp')) %>%
                   sf::st_transform(4269) )
  suppressMessages(hikessf <- sf::st_read(file.path(getwd(), 'trips', 'channel_preserve', 'data', 'hikes.shp')) %>%
                     sf::st_transform(4269) )
  

  trip_df <- read.csv(file.path(getwd(), 'data', 'created', 'test_trip.csv'))
  imagessf$URL <- trip_df$WebPage
  imagessf <- imagessf %>%
    mutate(label = paste(" <a href = ", URL, "/>Photo</a>")) 

  save.image(file = file.path(getwd(), 'trips', 'channel_preserve', 'data', 'mapdata.RData'))  
  