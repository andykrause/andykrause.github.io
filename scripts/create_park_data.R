#***************************************************************************************************
#
#  Script for making State Park Maps data
#
#***************************************************************************************************

  ## Set Parameters

  reload_state <- FALSE   # Do you to reload base map from census?

  ## Load Libraries

  library(tidyverse)
  library(tidycensus)
  library(sf)

  ## Load Custom Functions
  source(file.path(getwd(), 'scripts', 'mapping_functions.R'))

### Create Data ---------------------------------------------------------------------------------

  # County Boundaries
  if (reload_state){
  
    var <- 'B02001_001'
    census_api_key("eba9fd9d7bd7f7164ed95b4c7f8874cf37586d42")
    wa_sf <- tidycensus::get_acs(geography = 'county',
                                 state = 'wa',
                                 variables = var,
                                 year = 2018,
                                 geometry = T) %>%
      mutate(county = str_remove(str_remove(NAME, ' County.*'),
                               'Census Tract.*, ')) %>%
      mutate(state = str_remove(NAME, '.*County, ')) %>%
      select(-NAME) %>%
      dplyr::select(GEOID,county, geometry)
  
    saveRDS(wa_sf, file.path(getwd(), 'data', 'created', 'county_boundaries.RDS'))
  }
  
  # Points data (new Lat/Long)
  points_sf <- sf::st_read(file.path(getwd(), 'data', 'raw', 'waparks_points.shp')) %>%
    sf::st_transform(4269) 
  points_xy <- do.call(rbind, st_geometry(points_sf)) %>% 
    as_tibble() %>% 
    setNames(c("Longitude","Latitude"))
  points_sf$Longitude <- points_xy$Longitude
  points_sf$Latitude <- points_xy$Latitude

  # Park Table  
  data_df <- readr::read_csv(file.path(getwd(), 'data','created', 'final_parks_data.csv')) %>%
    #dplyr::filter(Challenge == 1) %>%
    dplyr::mutate(Visited = as.factor(Visited)) #%>%
    #dplyr::select(-c('Longitude', 'Latitude')) 
  #data_df$Longitude <- points_sf$Longitude[match(data_df$Abbrv, points_sf$Abbrv)]
  #data_df$Latitude <- points_sf$Latitude[match(data_df$Abbrv, points_sf$Abbrv)]
  saveRDS(data_df, file.path(getwd(), 'data','created', 'data.RDS'))
  
 points_sf <- points_sf %>%
    dplyr::inner_join(., data_df %>% dplyr::select(Abbrv, Visited, Weather), by = 'Abbrv')
  saveRDS(points_sf, file.path(getwd(), 'data','created', 'points.RDS'))
  
  # Park Polygons 
  boundaries_sf <- sf::st_read(file.path(getwd(), 'data', 'raw', 
                                              'waparks_boundaries.shp')) %>%
    sf::st_transform(4269) %>%
    dplyr::select(Name = ParkName, Abbrv, geometry) %>%
    dplyr::left_join(., data_df %>%
                       dplyr::select(Abbrv, Visited, Order, Date = Date_Visited, Weather),
                     by = 'Abbrv')
  saveRDS(boundaries_sf, file.path(getwd(), 'data', 'created', 'boundaries.RDS'))

  # Park Drives
  drives_sf <- sf::st_read(file.path(getwd(), 'data','raw', 'waparks_drives.shp')) %>%
    sf::st_transform(4269) %>%
    dplyr::left_join(., data_df %>%
                        dplyr::select(park = Abbrv, ParkName, ParkCode, Date_Visited, Visited),
                     by = 'park')
  saveRDS(drives_sf, 
          file.path(getwd(), 'data','created', 'drives.RDS'))
  
  # Park Ferries
  ferries_sf <- sf::st_read(file.path(getwd(), 'data','raw', 'waparks_ferries.shp')) %>%
    sf::st_transform(4269) %>%
    dplyr::left_join(., data_df %>%
                       dplyr::select(park = Abbrv, ParkName, ParkCode, Date_Visited, Visited),
                     by = 'park')
  saveRDS(ferries_sf, file.path(getwd(), 'data','created', 'ferries.RDS'))
  
  
  # Park Baoting
  boating_sf <- sf::st_read(file.path(getwd(), 'data','raw', 'waparks_boating.shp')) %>%
    sf::st_transform(4269) %>%
    dplyr::left_join(., data_df %>%
                       dplyr::select(park = Abbrv, ParkName, ParkCode, Date_Visited, Visited),
                     by = 'park')
  saveRDS(boating_sf, file.path(getwd(), 'data','created', 'boating.RDS'))
  
  # Park Baoting
  paddle_sf <- sf::st_read(file.path(getwd(), 'data','raw', 'waparks_paddleboarding.shp')) %>%
    sf::st_transform(4269) %>%
    dplyr::left_join(., data_df %>%
                       dplyr::select(park = Abbrv, ParkName, ParkCode, Date_Visited, Visited),
                     by = 'park')
  saveRDS(paddle_sf, file.path(getwd(), 'data','created', 'paddle.RDS'))
  
  # Park Baoting
  hikes_sf <- sf::st_read(file.path(getwd(), 'data','raw', 'waparks_hikes.shp')) %>%
    sf::st_transform(4269) %>%
    dplyr::left_join(., data_df %>%
                       dplyr::select(park = Abbrv, ParkName, ParkCode, Date_Visited, Visited),
                     by = 'park')
  saveRDS(hikes_sf, file.path(getwd(), 'data','created', 'hikes.RDS'))
  
  # Park Baoting
  stays_sf <- sf::st_read(file.path(getwd(), 'data','raw', 'waparks_stays.shp')) %>%
    sf::st_transform(4269)
  saveRDS(stays_sf, file.path(getwd(), 'data','created', 'stays.RDS'))
  
  
  
  



