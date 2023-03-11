#***************************************************************************************************
#
#       Script that builds maps for Kraustralia Blog and Book
#
#***************************************************************************************************

### Parameters -------------------------------------------------------------------------------------
 
  # How many days are ready
  kraustralia_days <- 3
  
  # Set basemap parameters
  basemap_par <- list(service = 'carto',
                      type = '',
                      res = 1,
                      token = Sys.getenv('mapbox_token'))
  
  # Set CRS
  k_crs <- 4269
  
  # Set mask buffer width
  k_buffer <- 1000

### Setup ------------------------------------------------------------------------------------------

  ## Load libraries
  library(sf)
  library(geosphere)
  library(tidyverse)
  library(basemaps)
  library(maps)
  library(cowplot)

  ## Source Custom Functions
  source(file.path(getwd(), 'functions', "mapping_functions.R"))

  ## Set Paths

  file_path <- '~/documents/_travel/kraustralia'
  image_path <- file.path(getwd(), 'kraustralia', 'images')
  output_path <- file.path(getwd(), 'kraustralia', 'maps')
  
### Load Geographical Data -------------------------------------------------------------------------
  
  ## Stays (points)
  stays_sf <- sf::st_read(file.path(file_path, 'kraustralia_stays.shp')) %>%
    sf::st_transform(k_crs) %>%
    dplyr::mutate(mode = 'Stays')
  
  ## Drives (lines)
  drives_sf <- sf::st_read(file.path(file_path, 'kraustralia_drives.shp')) %>%
      sf::st_transform(k_crs) %>%
      dplyr::mutate(mode = 'Drives')
  
  ## Walks/Hikes (lines)
  walks_sf <- sf::st_read(file.path(file_path, 'kraustralia_walks.shp')) %>%
    sf::st_transform(k_crs) %>%
    dplyr::mutate(mode = 'Walks')

  ## Rails (lines)
  rails_sf <- sf::st_read(file.path(file_path, 'kraustralia_rails.shp')) %>%
    sf::st_transform(k_crs) %>%
    dplyr::mutate(mode = 'Trains')

  ## Flights (lines)
    #airports_sf <- sf::st_read(file.path(file_path, 'kk_airport_points.shp')) %>%
    # sf::st_transform(4269) 
    #airports_sf <- cbind(airports_sf, airports_sf %>% sf::st_coordinates())

### Prepare Data -----------------------------------------------------------------------------------  
  
  day_geos <- purrr::map(.x = as.list(1:kraustralia_days),
                         .f = createDayGeometry,
                         line_geos = list(walks_sf, drives_sf, rails_sf),
                         stays_sf = stays_sf,
                         buff_dist = k_buffer,
                         crs = k_crs) %>%
    purrr::set_names(., paste0('blog_', 1:kraustralia_days))
  
### Create Maps ------------------------------------------------------------------------------------  

  
  
  
  
  
  
  
 
 # daygeo_$stay <- NULL
  day_plot <- plotDayGeos(daygeo_, mask_alpha = .7, map_service = 'osm_stamen',
                          map_type = 'terrain')
  lines_cols = c('purple', 'blue', 'red')
  day_plot  
  
###  SANDBOX ------- 
  ########################3

  
  png(file= file.path(image_path, "kraustralia_map_2.png"),
      width=250, height=450)
    day_plot
  dev.off()
  
  
  
  set_defaults(map_service = "carto", map_type = "dark", map_token = mp_token,
               map_res = 1)
  #basemap_mapview(daygeo_$bbox, map_res = 1)
  ggplot() + 
    theme_nothing() + 
    basemap_gglayer(daygeo_$bbox) +
    scale_fill_identity() + 
    coord_sf()+
    geom_sf(data = daygeo_$lines %>%
              sf::st_transform(., 3857), 
            aes(color = 3), size = 1, inherit.aes = FALSE) -> k
  
  ggsave(file.path(getwd(), 'test.png'), 
         k, dpi = 1000,
         width = 12,
         height = 7,
         bg='transparent')
  
  
  
  #https://jakob.schwalb-willmann.de/basemaps/
  
  library(basemaps)
  data(ext)
  # or use draw_ext() to interactively draw an extent yourself
  
  # view all available maps
  get_maptypes()
  basemap_magick(ext)
  basemap_magick(ext, map_service = "carto", map_type = "dark", map_res = 1)
  basemap_magick(ext, map_service = "esri", map_type = "world_dark_gray_reference")
  basemap_magick(ext, map_service = "mapbox", map_type = "dark",
                 map_token = mp_token)
  
  
  
  set_defaults(map_service = "mapbox", map_type = "dark", map_token = mp_token,
               map_res = 1)
  ggplot() + 
    basemap_gglayer(ext) +
    scale_fill_identity() + 
    coord_sf()
  
  basemap_mapview(ext)
  
  
  