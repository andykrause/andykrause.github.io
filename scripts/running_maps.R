####################################################################################################
#
#       Running Maps
#
####################################################################################################

### Setup --------------

 ## Load libraries
  library(sf)
  library(geosphere)
  library(tidyverse)
  library(basemaps)
  library(maps)
  library(tmap)

 ## Source Custom Functions
  source("~/projects/andykrause.github.io/functions/mapping_functions.R")

 ## Set Paths

  file_path <- '~/documents/_travel/america'
  image_path <- 'x'

### Load Geographical Data -------------

  ## Load Running Shapefile
  running_sf <- sf::st_read(file.path(file_path, 'running.shp')) %>%
    sf::st_transform(4269) 

### 

trailruns_sf <- running_sf %>%
  dplyr::filter(!is.na(type) & !is.na(state)) %>%
  dplyr::filter(type == 'trail') %>%
  dplyr::select(name, state, type)

roadruns_sf <- running_sf %>%
  dplyr::filter(!is.na(type) & !is.na(state)) %>%
  dplyr::filter(type != 'trail') %>%
  dplyr::select(name, state, type)

`Trail Runs` <- trailruns_sf
`Road Runs` <- roadruns_sf

## Seattle Area

bbox_sea <- st_bbox(trailruns_sf) # current bounding box
bbox_sea[1] <- -122.45
bbox_sea[2] <- 47.4
bbox_sea[3] <- -121.87
bbox_sea[4] <- 47.8

tmap_mode("view")

tm_shape(`Trail Runs`, bbox = bbox_sea) + 
  tm_lines(col = 'red', scale = 1, legend.lwd.show = TRUE) +
  tm_shape(`Road Runs`, bbox = bbox_sea) + 
    tm_lines(col = 'navajowhite2', scale = 1, legend.lwd.show = FALSE)
  
### San Juans

bbox_sji <- st_bbox(trailruns_sf) # current bounding box
bbox_sji[1] <- -123.17
bbox_sji[2] <- 48.4
bbox_sji[3] <- -122.59
bbox_sji[4] <- 48.68

tmap_mode("view")

tm_shape(`Trail Runs`, bbox = bbox_sji) + 
  tm_lines(col = 'red', scale = 1, legend.lwd.show = TRUE) +
  tm_shape(`Road Runs`, bbox = bbox_sji) + 
  tm_lines(col = 'navajowhite2', scale = 1, legend.lwd.show = FALSE)

### Mazama

bbox_maz <- st_bbox(trailruns_sf) # current bounding box
bbox_maz[1] <- -120.53
bbox_maz[2] <- 48.50
bbox_maz[3] <- -120.28
bbox_maz[4] <- 48.66

tmap_mode("view")

tm_shape(`Trail Runs`, bbox = bbox_maz) + 
  tm_lines(col = 'red', scale = 1, legend.lwd.show = TRUE) +
  tm_shape(`Road Runs`) + 
  tm_lines(col = 'navajowhite2', scale = 1, legend.lwd.show = FALSE)

### Hawaii

bbox_haw <- st_bbox(wa_trail_sf) # current bounding box
bbox_haw[1] <- -156.10
bbox_haw[2] <- 19.70
bbox_haw[3] <- -155.61
bbox_haw[4] <- 20.06

tmap_mode("view")

tm_shape(`Trail Runs`, bbox = bbox_haw) + 
  tm_lines(col = 'red', scale = 1, legend.lwd.show = TRUE) +
  tm_shape(`Road Runs`) + 
  tm_lines(col = 'navajowhite2', scale = 1, legend.lwd.show = FALSE)


