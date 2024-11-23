#***************************************************************************************************
#
#  Prepare Data For Running Maps and GAIA
#
#***************************************************************************************************

### Setup ----------------------------------------------------------------------

  ## Load libraries
  library(sf)
  library(geosphere)
  library(tidyverse)

  ## Source Custom Functions
  source(file.path(getwd(), "functions/mapping_functions.R"))

  ## Set Paths

  data_path <- '~/dropbox/andy/data/where/america'
  output_path <- '~/dropbox/andy/data/projects/running_maps/'

### Load Data ------------------------------------------------------------------  
  
  ## Load Running Shapefile
  running_sf <- sf::st_read(file.path(data_path, 'running.shp'),
                            quiet=TRUE) %>%
    sf::st_transform(4269) 

  ## Split into trail/road 

  trailruns_sf <- running_sf %>%
    dplyr::filter(!is.na(type)) %>%
    dplyr::filter(type == 'trail') 

  roadruns_sf <- running_sf %>%
    dplyr::filter(!is.na(type)) %>%
    dplyr::filter(type != 'trail')

### Write to project location --------------------------------------------------
  
  sf::st_write(trailruns_sf, 
               file.path(output_path, 'trail_runs.shp'), 
               append = FALSE)
  
  sf::st_write(roadruns_sf, 
               file.path(output_path, 'road_runs.shp'), 
               append = FALSE)

################################################################################
### TEST, CUTTING UP FOR GAIA

if(F){    
  
 
 #      
 # trail_coords <- sf::st_coordinates(trailruns_sf)
 # 
 # trail_cuts <- floor(nrow(trail_coords)/1000) + 1
# 
# a <- 1
# b <- 1
# 
# for(i in 1:trail_cuts){
#   b <- trail_coords[i*1000,4]-1
#   fil <- paste0('trail_', i, '.kml')
#   sf::st_write(trailruns_sf$geometry[a:b], paste0('~/dropbox/andy/data/where/gaia/', fil),
#                append=FALSE)
#   a <- b+1
# }
# 
# 
# head(h)
 
 
}