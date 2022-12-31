####################################################################################################
#
#       Script that builds maps for Kiwis and Kangaroos Blog
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
  
  file_path <- '~/documents/_travel/kiwiskangaroos'
  image_path <- 


# Park Boating
ped_sf <- sf::st_read(file.path(file_path, 'kk_ped.shp')) %>%
  sf::st_transform(4269) %>%
  dplyr::select(Desc, day, osm_id, full_id) %>%
  dplyr::mutate(mode = 'Walk')
train_sf <- sf::st_read(file.path(file_path, 'kk_rail.shp')) %>%
  sf::st_transform(4269) %>%
  dplyr::select(Desc, day, osm_id, full_id) %>%
  dplyr::mutate(mode = 'Train')
stays_sf <- sf::st_read(file.path(file_path, 'kk_stays.shp')) %>%
  sf::st_transform(4269) %>%
  dplyr::mutate(mode = 'Stays')
airports_sf <- sf::st_read(file.path(file_path, 'kk_airport_points.shp')) %>%
  sf::st_transform(4269) 
airports_sf <- cbind(airports_sf, airports_sf %>% sf::st_coordinates())


daygeo_ <- createDayGeometry(day = 1,
                             lines_geos = list(ped_sf, train_sf),
                             stays_sf = stays_sf,
                             buff_dist = 1200)

day_plot <- plotDayGeos(daygeo_, mask_alpha = .7, map_service = 'osm_stamen',
                        map_type = 'terrain')
day_plot


daygeo_ <- createDayGeometry(day = 2,
                             lines_geos = list(ped_sf, train_sf),
                             stays_sf = stays_sf,
                             buff_dist = 1800)

day_plot <- plotDayGeos(daygeo_, mask_alpha = .7, map_service = 'carto',
                        map_type = 'dark')
day_plot

## Day 0 Flights

flights_0 <- list(c('TUS', "LAX"),
                  c('LAX', 'NAN'),
                  c('NAN', 'BNE'))

purrr::map(.x = flights_0,
           .f = flightPath,
           air_sf = airports_sf)  ->
  flight_paths

x <- flight_paths[[1]]



for (i in 1:(nrow(x)-1)){
  
  if (i == 1){
    x_df = data.frame(id = i,
                      sx = x$lon[i],
                      ex = x$lon[i+1],
                      sy = x$lat[i],
                      ey = x$lat[i+1])
  } else {
    x_df <- 
      dplyr::bind_rows(x_df, 
                       data.frame(id = i,
                                  sx = x$lon[i],
                                  ex = x$lon[i+1],
                                  sy = x$lat[i],
                                  ey = x$lat[i+1]))
  }
}

# dfr_sf <- sf::st_as_sf(x_df, coords = c(c("sx", "ex"), 
#                                    c("sy", "ey")), 
#                    crs = "+proj=longlat +datum=WGS84")

ls <- apply(x_df, 1, function(x) 
{
  v <- as.numeric(x[c(3,5,2,4)])
  m <- matrix(v, nrow = 2)
  return(sf::st_sfc(sf::st_linestring(m), crs = 4326))
})
ls = Reduce(c, ls)
ls$ID = x_df$ID 
ls_sf <- sf::st_as_sf(ls, data = x_df)  




if(F){
# 
# 
# all_sf <- dplyr::bind_rows(ped_sf, train_sf)
# 
# buff_sf <- sf::st_buffer(all_sf, dist = 1000) %>%
#   sf::st_make_valid()
# 
# buffu_sf <- sf::st_union(buff_sf) %>%
#   sf::st_make_valid()
# 
# sf::st_is_valid(sf::st_set_precision(buff_sf, 1e6)) 
# 
# sf::st_is_valid(sf::st_make_valid(sf::st_set_precision(buff_sf, 1e6))) 
# 
# allc_sf <- dplyr::bind_rows(all_sf, buff_sf)
# 
# coords_cf <- sf::st_coordinates(all_sf) %>%
#   as.data.frame()
# 
# cc_cf <- data.frame(X = c(min(coords_cf$X)* .95,
#                           max(coords_cf$X)*1.05),
#                     Y = c(min(coords_cf$Y)* .95,
#                           max(coords_cf$Y)*1.05))
# 
# 
# mask_sf <- cc_cf %>% 
#   sf::st_as_sf(coords = c("X", "Y"), 
#            crs = 4269) %>% 
#   sf::st_bbox() %>% 
#   sf::st_as_sfc()
# 
# x_sf <- sf::st_difference(mask_sf, buffu_sf)
# 
# 
# 
# ggmap::ggmap(basemap) + 
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
#   xlab('') +
#   ylab('') +
#   #geom_sf(data = ped_sf, aes(color = day), inherit.aes = FALSE) +
#   #geom_sf(data = train_sf, aes(color = day), inherit.aes = FALSE) +
#   geom_sf(data = all_sf, aes(color = mode), size = 2, inherit.aes = FALSE) +
#   #geom_sf(data = buff_sf[1:34, ], aes(fill=day), color = 'red', inherit.aes = FALSE) + 
#   geom_sf(data = x_sf, color = 'white', fill = 'white', inherit.aes=FALSE) 
# 
# 
# 


#ggplot(ped_sf) + 
#  geom_sf(aes(color = day))





# World map is available in the maps package
library(maps)
library(geosphere)

# No margin
par(mar=c(0,0,0,0))

# World map
maps::map('world',
    col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
    mar=rep(0,4),border=0, ylim=c(-55,-1), xlim = c(110, 180) 
)
points(x=air_sf$X, y=air_sf$Y, col="slateblue", cex=3, pch=20)



fff <- list(c('BNE', "SYD"),
            c('SYD', 'AKL'),
            c('CHC', 'MEL'),
            c('PER', 'DRW'),
            c('PER', 'BNE'))

purrr::map(.x = fff,
           .f = flightPath,
           air_sf = air_sf)  ->
  flight_paths

# Show this connection
for (k in 1:length(flight_paths)){
  lines(flight_paths[[k]], col="slateblue", lwd=2)
}




# Compute the connection between Buenos Aires and Paris
inter <- gcIntermediate(as.double(air_sf[1,c('X','Y')])[1:2],  
                        as.double(air_sf[2, c('X','Y')])[1:2], 
                        n=50, addStartEnd=TRUE, breakAtDateLine=F)


# Between Paris and Melbourne
inter <- gcIntermediate(Melbourne,  Paris, n=50, addStartEnd=TRUE, breakAtDateLine=F)             
lines(inter, col="slateblue", lwd=2)

}