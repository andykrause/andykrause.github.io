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
  
  line_geos = list(walks_sf, drives_sf, rails_sf)
  
  createDayGeometry <- function(day_nbr,
                                line_geos,
                                point_geos,
                                buff_dist = 1000,
                                crs = 4269,
                                ...){
    
    ## Combine lines
    for (i in 1:length(line_geos)){
      line_sf <- line_geos[[i]] %>%
        dplyr::filter(day == day_nbr)
      if (i == 1){
        lines_sf <- line_sf
      } else {
        lines_sf <- lines_sf %>%
          dplyr::bind_rows(line_sf)
      }
    }
    
    ## Create Buffer
    buff_sf <- sf::st_buffer(lines_sf, dist = buff_dist) %>%
      sf::st_make_valid()
    
    buff_sf <- buff_sf %>%
      sf::st_union() %>%
      sf::st_make_valid() 
    
    if (! 'data.frame' %in% class(buff_sf)){
      pc <- unlist(lapply(buff_sf, function(x) unlist(class(x)[2])))
      buff_sf <- buff_sf[[which(pc == 'POLYGON')]] %>%
        sf::st_sfc() %>%
        sf::st_sf() %>%
        sf::st_set_crs(., crs)
    }
    
    ## Create Mask
    coords_cf <- data.frame(corners = as.vector(sf::st_bbox(buff_sf)))
    x_dif <- abs(coords_cf$corners[3] - coords_cf$corners[1])
    y_dif <- abs(coords_cf$corners[4] - coords_cf$corners[2])
    
    
    coordbounds_cf <- data.frame(X = c(coords_cf$corners[1] - x_dif * .05,
                                       coords_cf$corners[3] + x_dif * .05),
                                 Y = c(coords_cf$corners[2] - y_dif * .05,
                                       coords_cf$corners[4] + y_dif * .05))
    bounding_sf <- coordbounds_cf %>% 
      sf::st_as_sf(coords = c("X", "Y"), 
                   crs = 4269)
    
    poly_sf <- bounding_sf %>% 
      sf::st_bbox() %>% 
      sf::st_as_sfc()
    
    mask_sf <- sf::st_difference(poly_sf, buff_sf) 
    
    ## Create Stays
    stay_sf <- stays_sf %>%
      dplyr::filter(NightStart <= day_filter & NightEnd >= day_filter) %>%
      dplyr::arrange(NightEnd) %>%
      dplyr::mutate(NightOrder = 1:dplyr::n(),
                    StayColor = ifelse(NightOrder == 1, 'gray20', 'black'))
    
    
    return(
      list(bbox = bounding_sf,
           lines = lines_sf,
           mask = mask_sf,
           stay = stay_sf,
           coords = coords_cf))
  }
  
  
  
  
  
  
  
  
  
  
  
  daygeo_ <- createDayGeometry(day = 2,
                               lines_geos = list(drives_sf, walks_sf, rails_sf),
                               stays_sf = stays_sf,
                               buff_dist = 1200)
  

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
  
  
  