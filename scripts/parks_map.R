
library(tidyverse)
library(tidycensus)
library(sf)
library(OpenStreetMap)


  county_visit = c('king', 'snohomish', 'kittitas', 'island', 'san juan', 'kitsap', 'pierce',
                   'skagit', 'thurston')

  if (F){
     var <- 'B02001_001'
     census_api_key("eba9fd9d7bd7f7164ed95b4c7f8874cf37586d42")
      wa_sf <- tidycensus::get_acs(geography = 'county',
                                   state = 'wa',
                                   variables = var,
                               year = 2018,
                               geometry = T)
      
      wa_sf <- 
        wa_sf %>%
        mutate(county = str_remove(str_remove(NAME, ' County.*'),
                                   'Census Tract.*, ')) %>%
        mutate(state = str_remove(NAME, '.*County, ')) %>%
        select(-NAME) %>%
        dplyr::select(GEOID,county, geometry)
      
      saveRDS(wa_sf, file.path(getwd(), 'data', 'county_boundaries.RDS'))
      
      parks_poly_sf <- sf::st_read(file.path(getwd(), 'data', 'State_parks.shp')) %>%
        sf::st_transform(., crs = 4269)
      sf::st_crs(parks_poly_sf) <- 4269
      
      saveRDS(parks_poly_sf, file.path(getwd(), 'data', 'park_boundaries.RDS'))
      
  } else {
    
    wa_sf <- readRDS(file.path(getwd(), 'data', 'county_boundaries.RDS'))
    parks_df <- readr::read_csv(file.path(getwd(), 'data', 'final_parks_data.csv')) %>%
      dplyr::filter(Challenge == 1) %>%
      dplyr::mutate(Visited = as.factor(Visited))
    parks_sf <- sf::st_as_sf(parks_df, coords = c('Longitude', 'Latitude'))
    st_crs(parks_sf) <- 4269
    parks_poly_sf <- readRDS(file.path(getwd(), 'data', 'park_boundaries.RDS'))
  }
  
  
  wa_sf <- wa_sf %>%
    mutate(visited = as.factor(ifelse(tolower(county) %in% county_visit, 1, 0)))
  
 state_map <-   
 ggplot(data = wa_sf) + 
   geom_sf(aes(fill = visited), fill = 'gray90', color = 'white') + 
   scale_fill_manual(name = 'Visited', values = c('white', 'gray70')) + 
   theme(axis.title=element_blank(),
         axis.text=element_blank(),
         axis.ticks=element_blank(),
         panel.border = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         legend.position = 'none') + 
   geom_sf(data = parks_sf, aes(color = Visited, size = Visited, shape = Visited)) + 
   scale_color_manual(values = c('gray10', 'green4')) + 
   scale_size_manual(values = c(.8, 2)) + 
   scale_shape_manual(values = c(20, 17))
 
 
 png(filename = file.path(getwd(), 'images', 'state_map.png'), width = 780, height = 480)
   state_map
 dev.off()
 
 
 ####
 
 # Blind Island
 
 plotPark <- function(park_code,
                      zoom = 11,
                      scale = .2,
                      map_type = 'toner-background',
                      park_color = 'green4'){
   
   park_df <- parks_df[parks_df$ParkCode == park_code, ]
   park_sf <- parks_poly_sf[parks_poly_sf$ParkCode == park_code, ]
 
   bbox <- c(left = park_df$Longitude - scale,
             bottom = park_df$Latitude - scale/2,
             right = park_df$Longitude + scale,
             top = park_df$Latitude + scale/2)
 
   basemap <- ggmap::get_stamenmap(bbox, zoom=zoom, maptype = map_type)
 
   ggmap(basemap) + 
     geom_sf(data = park_sf, aes(fill=ParkName), inherit.aes = FALSE, color = park_color) +
     xlab('') + 
     ylab('') + 
     scale_fill_manual(name = '', values = park_color) + 
     theme(legend.position = 'bottom',
           axis.text.y = element_blank(),
           axis.text.x = element_blank(),
           axis.ticks = element_blank())
 
 }
 
 ## Cama Beach
 png(filename = file.path(getwd(), 'images', 'map_spencerspit.png'), width = 780, height = 480)
   plotPark(parks_sf$ParkCode[grep('Spencer', parks_sf$ParkName)],
            scale=.28, map_type = 'toner', park_color = 'red')
 dev.off()
 

 
 
 
 