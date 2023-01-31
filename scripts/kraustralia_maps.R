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
  image_path <- file.path(getwd(), 'kraustralia', 'images')
  
### Load Geographical Data -------------
  
stays_sf <- sf::st_read(file.path(file_path, 'kraustralia_stays.shp')) %>%
  sf::st_transform(4269) %>%
  dplyr::mutate(mode = 'Stays')
  
drives_sf <- sf::st_read(file.path(file_path, 'kraustralia_drives.shp')) %>%
    sf::st_transform(4269) %>%
    dplyr::mutate(mode = 'Drives')
  
walks_sf <- sf::st_read(file.path(file_path, 'kraustralia_walks.shp')) %>%
  sf::st_transform(4269) %>%
  dplyr::mutate(mode = 'Walks')

rails_sf <- sf::st_read(file.path(file_path, 'kraustralia_rails.shp')) %>%
  sf::st_transform(4269) %>%
  dplyr::mutate(mode = 'Trains')


#airports_sf <- sf::st_read(file.path(file_path, 'kk_airport_points.shp')) %>%
#  sf::st_transform(4269) 
#airports_sf <- cbind(airports_sf, airports_sf %>% sf::st_coordinates())
  
  
  
  daygeo_ <- createDayGeometry(day = 5,
                               lines_geos = list(drives_sf, walks_sf, rails_sf),
                               stays_sf = stays_sf,
                               buff_dist = 1200)
  
  
  
 # daygeo_$stay <- NULL
  day_plot <- plotDayGeos(daygeo_, mask_alpha = .7, map_service = 'osm_stamen',
                          map_type = 'terrain')
  lines_cols = c('purple', 'blue', 'red')
  day_plot  
  
  
  
  png(file= file.path(image_path, "kraustralia_map_2.png"),
      width=250, height=450)
    day_plot
  dev.off()
