####################################################################################################
#
#       Script that builds maps for Kraustralia Blog and Book
#
##############################################################################################

### Setup --------------

  ## Load libraries
  library(sf)
  library(geosphere)
  library(tidyverse)
  library(basemaps)
  library(maps)

  ## Source Custom Functions
  source("~/projects/andykrause.github.io/scripts/mapping_functions.R")

  ## Set Paths

  file_path <- '~/documents/_travel/kraustralia'
  image_path <- 'x'
  
### Load Geographical Data -------------
  
stays_sf <- sf::st_read(file.path(file_path, 'kraustralia_stays.shp')) %>%
  sf::st_transform(4269) %>%
  dplyr::mutate(mode = 'Stays')
  
drives_sf <- sf::st_read(file.path(file_path, 'kraustralia_drives.shp')) %>%
    sf::st_transform(4269) %>%
    dplyr::mutate(mode = 'Drives')
  
#airports_sf <- sf::st_read(file.path(file_path, 'kk_airport_points.shp')) %>%
#  sf::st_transform(4269) 
#airports_sf <- cbind(airports_sf, airports_sf %>% sf::st_coordinates())
  
  
  
  daygeo_ <- createDayGeometry(day = -1,
                               lines_geos = list(drives_sf),
                               stays_sf = stays_sf,
                               buff_dist = 1200)
  
  day_plot <- plotDayGeos(daygeo_, mask_alpha = .7, map_service = 'osm_stamen',
                          map_type = 'terrain')
  day_plot  
  
