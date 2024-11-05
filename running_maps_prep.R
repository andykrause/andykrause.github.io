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
  
  # Get xy
  xy <- sf::st_coordinates(running_sf)
  xy_df <- data.frame(x = round(xy[,1], 1),
                      y = round(xy[,2], 1),
                      segment = xy[,4]) %>%
    dplyr::mutate(xy = paste0(x, '_', y))
  
  xy_cdf <- xy_df %>%
    dplyr::group_by(xy) %>%
    dplyr::summarise(xycount = dplyr::n()) %>%
    dplyr::arrange(desc(xycount)) %>%
    dplyr::inner_join(xy_df %>%
                        dplyr::distinct(x,y,xy))
  head(xy_cdf)
  
  i <- 1
  x1 <- xy_cdf$x[i] -.05
  x2 <- xy_cdf$x[i] +.05
  y1 <- xy_cdf$y[i] -.05
  y2 <- xy_cdf$y[i] +.05
  
  boundary_osm <- osmak::osmakQuery(c(x1, y1, x2, y2),
                                    'boundary')
  leisure_osm <- osmak::osmakQuery(c(x1, y1, x2, y2),
                                   'leisure')
  path_osm <- osmak::osmakQuery(c(x1, y1, x2, y2),
                                'highway')
  
  polys_sf <- dplyr::bind_rows(boundary_osm@polygons,
                               leisure_osm@polygons) %>%
    dplyr::distinct(osm_id, .keep_all = TRUE) %>%
    dplyr::select(osm_id, name, admin_level, boundary, source, geometry) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::filter(name != '')
  
  polys_sf$area <- sf::st_area(polys_sf)
  polys_sf <- polys_sf %>%
    dplyr::arrange(desc(area))
  
  library(osmak)
  
  boundary_osm <- osmak::osmakQuery(c(-122.2,47.5,-122,47.6),
                                    'boundary')
  
  leisure_osm <- osmak::osmakQuery(c(-122.2,47.5,-122,47.6),
                                   'leisure')
  
  path_osm <- osmak::osmakQuery(c(-122.2,47.5,-122,47.6),
                                   'highway')
  
  
  polys_sf <- dplyr::bind_rows(boundary_osm@polygons,
                               leisure_osm@polygons) %>%
    dplyr::distinct(osm_id, .keep_all = TRUE) %>%
    dplyr::select(osm_id, name, admin_level, boundary, source, geometry) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::filter(name != '')
  
  polys_sf$area <- sf::st_area(polys_sf)
  polys_sf <- polys_sf %>%
    dplyr::arrange(desc(area))
  
  
  paths_sf <- path_osm@lines %>%
    dplyr::filter(highway %in% c('path', 'track', 'footway')) %>%
    dplyr::select(name, highway, surface, geometry)
  
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
  
  
  gaia_list <- splitForGaia(z_sf, 1000)
  
splitForGaia <- function(x_sf, 
                         limit = 1000){
  
  x_coords <- sf::st_coordinates(x_sf)
  x_cuts <- floor(nrow(x_coords) / limit) + 1
  
  x_ <- list()
  
  begin <- end <- 1

  for(i in 1:x_cuts){
    
    if (i * limit > nrow(x_coords)) {
      ii <- nrow(x_coords) / limit } 
    else {
      ii <- i
    }
    
    end <- x_coords[ii * limit, 4] - 1
    x_[[i]] <- x_sf$geometry[a:b]
    begin <- end + 1
  }
  x_
}  
  
  ## Take coords of all run points, then find which lat/long to 1 decimal have most, work on those areas. 
  
x_sf <- polys_sf %>% 
  dplyr::filter(grepl("Squak Mountain", name))

x_sf <- sf::st_transform(x_sf, sf::st_crs(trailruns_sf))
xx_sf <- sf::st_buffer(x_sf, 250)

z_sf <- sf::st_intersection(trailruns_sf, x_sf) %>%
  suppressWarnings() %>%
  sf::st_cast('MULTILINESTRING')

p_sf <- sf::st_intersection(paths_sf, x_sf) %>%
  suppressWarnings() %>%
  sf::st_cast('MULTILINESTRING')

library(tmap)
tmap_mode("view")
tm_shape(x_sf) + tm_polygons() + 
  tm_shape(paths_sf) + tm_lines(col='red')+
  tm_shape(z_sf) + tm_lines(col='darkgreen', lwd=3)
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