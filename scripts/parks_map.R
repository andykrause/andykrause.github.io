#***************************************************************************************************
#
#  Script for making State Park Maps
#
#***************************************************************************************************

  ## Set Parameters
  
  reload_state <- FALSE   # Do you to reload base map from census?

  ## Load Libraries
  
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(OpenStreetMap)
  library(ggmap)

  ## Load Custom Functions
  source(file.path(getwd(), 'scripts', 'mapping_functions.R'))
  
### State Wide Map ---------------------------------------------------------------------------------
  
  ## Load Data 
  
  if (reload_state){
    
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
  
 ## Create Map
  
 state_map <-   
   ggplot(data = wa_sf) + 
     geom_sf(fill = 'gray90', color = 'white') + 
     theme(axis.title=element_blank(),
           axis.text=element_blank(),
           axis.ticks=element_blank(),
           panel.border = element_blank(), 
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(), 
           legend.position = 'none') + 
     geom_sf(data = parks_sf, aes(color = Visited, size = Visited, shape = Visited),
             alpha = .8) + 
     scale_color_manual(values = c('gray10', 'green4')) + 
     scale_size_manual(values = c(1.1, 2.6)) + 
     scale_shape_manual(values = c(20, 20))
 
  ## Save to file
  png(filename = file.path(getwd(), 'images', 'state_map.png'), width = 780, height = 480)
    state_map
  dev.off()
 
### Individual Park Maps ---------------------------------------------------------------------------
 
 ## Set Parameters
 
  image_width <- 800
  image_height <- 800
  cp_scale <- 500 # Width of boundary to create circle
  c_scale <- 1.6  # Scale of lat to long to create square
  x_scale <- .15 # Map zoom scale (width in lon)

  ## Limit to visited
  
  visited_sf <- parks_sf %>%
     dplyr::filter(Visited == 1) %>%
     dplyr::mutate(Date_Visited = as.Date(Date_Visited, '%m/%d/%y')) %>%
     dplyr::arrange(desc(Date_Visited))
  
  ## Print Maps
  
  #for (k in 1:nrow(visited_sf)){
  for (k in 35:37){
 
     i_sf <- visited_sf[k,]
     cat('Drawing: ', i_sf$ParkName, '\n')
     shortname <- tolower(i_sf$ParkName)
     shortname <- gsub(' ', '', shortname)
     png(filename = file.path(getwd(), 'images', paste0('map_', shortname, '.png')), 
                              width = image_width, height = image_height)
       plotPark(i_sf$ParkCode,
                scale=x_scale, map_type = 'toner', park_color = 'red', 
                circle = TRUE, cscale = c_scale, cpscale=cp_scale)
     dev.off()
  }
  
### Create summary table ---------------------------------------------------------------------------
  
  summ_df <- 
   data.frame(
    Name = c('Total', 'Visited', 'Remaining', 'Miles Driven', 'Miles Ferried', 
             'Miles Boated', 'Miles Hiked', 'Miles Paddleboarded'),
    Measure = c(nrow(parks_sf),
                sum(as.numeric(visited_sf$Challenge)),
                nrow(parks_sf) - sum(as.numeric(visited_sf$Challenge)),
                sum(as.numeric(visited_sf$Driven), na.rm = TRUE),
                sum(as.numeric(visited_sf$Ferried), na.rm = TRUE),
                sum(as.numeric(visited_sf$Boated), na.rm = TRUE),
                sum(as.numeric(visited_sf$Hiked), na.rm = TRUE),
                sum(as.numeric(visited_sf$Paddleboarded), na.rm = TRUE))) %>%
    dplyr::mutate(Measure = round(Measure, 0))
  
  saveRDS(summ_df, file.path(getwd(), 'images', paste0('summary.RDS')))
  saveRDS(visited_sf, file.path(getwd(), 'images', paste0('visited.RDS')))
  
  
#***************************************************************************************************
#***************************************************************************************************
  
  
  