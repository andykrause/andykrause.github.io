#******************************************************************************* 
#
#  Sandbox for Trail Coverage Analysis
#
#******************************************************************************* 

### Setup ----------------------------------------------------------------------

  ## Load libraries
  library(sf)
  library(geosphere)
  library(tidyverse)
  library(osmak)

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

  ## Filter to Train Runs
  trailruns_sf <- running_sf %>%
    dplyr::filter(!is.na(type)) %>%
    dplyr::filter(type == 'trail') 

  ## Filter to Road Runs
  roadruns_sf <- running_sf %>%
    dplyr::filter(!is.na(type)) %>%
    dplyr::filter(type != 'trail')

### Get OSM Data ---------------------------------------------------------------
  
 ## Get grids for all runs
  
  # Extract XYs, put into a DF with concatenated Lat_Long
  xy_coords <- sf::st_coordinates(running_sf)
  xy_df <- data.frame(x = round(xy_coords[, 1], 1),
                      y = round(xy_coords[, 2], 1),
                      segment = xy_coords[, 4]) %>%
    dplyr::mutate(xy = paste0(x, '_', y))

  # Create counts of which lat/long grid have most segments
  xy_cdf <- xy_df %>%
    dplyr::group_by(xy) %>%
    dplyr::summarise(xy_count = dplyr::n()) %>%
    dplyr::arrange(desc(xy_count)) %>%
    dplyr::inner_join(xy_df %>%
                        dplyr::distinct(x, y, xy),
                      by = 'xy')
  
  # Debug Reporting
  head(xy_cdf)
  cat('Total of',nrow(xy_cdf), 'lat/long grids\n')

 ## Get Data for a single grid  
  
  # Select Grid (Cougar MT in this example)
  i <- which(xy_cdf$x == -122.1 & xy_cdf$y == 47.5)

  # Get grid bounding box
  gbb <- pointToBbox(xy_cdf$x[i],
                     xy_cdf$y[i], 
                     offset = 0.05)

  # Boundary Data
  boundary_osm <- osmak::osmakQuery(gbb, 'boundary')

  # Leisure Data
  leisure_osm <- osmak::osmakQuery(gbb, 'leisure')

  # Path Data
  route_osm <- osmak::osmakQuery(gbb, 'highway')

 ## Prepare Polygon OSM data  
  
  # Combine, dedup, select and filter to those with names
  polys_sf <- dplyr::bind_rows(boundary_osm@polygons,
                               leisure_osm@polygons) %>%
    dplyr::distinct(osm_id, .keep_all = TRUE) %>%
    dplyr::select(osm_id, name, admin_level, boundary, source, geometry) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::filter(name != '')

  # Add Area calculations, order by size
  polys_sf$area <- sf::st_area(polys_sf)
  polys_sf <- polys_sf %>%
    dplyr::arrange(desc(area))
  asd(polys_sf, 3)
  

####------------  
  
   ## Prepare Route OSM data  
  
  # Select to needed fields
  routes_sf <- path_osm@lines %>%
    dplyr::select(name, highway, access, surface, geometry) %>%
    dplyr::mutate(paved = ifelse(is.na(surface), 1, 
                                 ifelse(surface %in% 
                                          c('asphalt', 'concrete', 'paved'),
                                   1, 0)),
                  private = ifelse(is.na(access), 0, 
                                   ifelse(access %in% 
                                            c('customers', 'no', 
                                              'permissive', 'private'), 
                                          0, 1))) %>%
    dplyr::select(name, highway, private, paved, geometry)
  head(routes_sf)
  

  
  
  routes_sf <- path_osm@lines %>%
    dplyr::filter(highway %in% c('path', 'track', 'footway')) %>%
    
x_sf <- polys_sf %>% 
  dplyr::filter(grepl("Cougar Mountain", name))

x_sf <- sf::st_transform(x_sf, sf::st_crs(trailruns_sf))
xx_sf <- sf::st_buffer(x_sf, 250)

z_sf <- sf::st_intersection(trailruns_sf, x_sf) %>%
  suppressWarnings() %>%
  sf::st_cast('MULTILINESTRING')

paths_sf <- sf::st_transform(paths_sf, sf::st_crs(trailruns_sf))
p_sf <- sf::st_intersection(paths_sf, x_sf) %>%
  suppressWarnings() %>%
  sf::st_cast('MULTILINESTRING')

library(tmap)
tmap_mode("view")
tm_shape(x_sf) + tm_polygons() + 
  tm_shape(p_sf) + tm_lines(col='red')+
  tm_shape(z_sf) + tm_lines(col='darkgreen', lwd=3)

##TODO:  Remove small routes that don't connect
##TODO:  Set terminology, Tracks, Routes, Paths....

# 
# gaia_list <- splitForGaia(z_sf, 1000)
# 
# splitForGaia <- function(x_sf, 
#                          limit = 1000){
#   
#   x_coords <- sf::st_coordinates(x_sf)
#   x_cuts <- floor(nrow(x_coords) / limit) + 1
#   
#   x_ <- list()
#   
#   begin <- end <- 1
#   
#   for(i in 1:x_cuts){
#     
#     if (i * limit > nrow(x_coords)) {
#       ii <- nrow(x_coords) / limit } 
#     else {
#       ii <- i
#     }
#     
#     end <- x_coords[ii * limit, 4] - 1
#     x_[[i]] <- x_sf$geometry[a:b]
#     begin <- end + 1
#   }
#   x_
# }  

 