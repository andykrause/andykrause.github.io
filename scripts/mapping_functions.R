#***************************************************************************************************
#
#  Functions to create park maps
#
#***************************************************************************************************


makeCircle <- function(center_sf, 
                       radius, 
                       nPoints = 1000){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(center_sf$Latitude)
  # length per longitude changes with lattitude, so need correction
  radiusLon <- radius / cos(meanLat/57.3) 
  radiusLat <- radius
  circleDF <- data.frame(ID = rep(1, each = nPoints))
  angle <- seq(0, 2*pi, length.out = nPoints)
  
  circleDF$lon <- center_sf$Longitude + radiusLon * cos(angle)
  circleDF$lat <- center_sf$Latitude + radiusLat * sin(angle)
  return(circleDF)
}

# Function
plotPark <- function(abbr,
                     zoom = 11,
                     scale = .2,
                     lscale = 1.6,
                     map_type = 'toner-background',
                     park_color = 'green4',
                     circle = FALSE,
                     cscale = 2,
                     cpscale = 80,
                     spin = 1){
  
  park_df <- data_df[which(data_df$Abbrv == abbr), ]
  park_sf <- boundaries_sf[which(boundaries_sf$Abbr == abbr), ]

    
  bbox <- c(left = park_df$Longitude - scale,
            bottom = park_df$Latitude - scale/lscale,
            right = park_df$Longitude + scale,
            top = park_df$Latitude + scale/lscale)
  
  basemap <- ggmap::get_stamenmap(bbox, zoom=zoom, maptype = map_type)
  
  base_plot <- 
    ggmap::ggmap(basemap) + 
    geom_sf(data = park_sf, aes(fill = Name), inherit.aes = FALSE, color = park_color) +
    xlab('') + 
    ylab('') + 
    scale_fill_manual(name = '', values = park_color) + 
    theme(legend.position = 'none',
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank())
  
  if(circle){
    circle_sf <- makeCircle(park_df, scale/cscale)
    
    base_plot <- 
      base_plot + 
      geom_polygon(data = circle_sf,
                   aes(x=lon, y = lat), fill = NA, color = 'white',
                   size = scale * cpscale) 
  }
  base_plot
} 

cropp <- function(im, left = 0, top = 0, right = 0, bottom = 0) {
  d <- dim(im[[1]]); w <- d[2]; h <- d[3]
  magick::image_crop(im, glue::glue("{w-left-right}x{h-top-bottom}+{left}+{top}"))
}

makeTblx <- function(visited_sf, 
                     park_name){
  
  tbl <- visited_sf %>%
    dplyr::filter(ParkName == park_name)
  
  traveled <- tbl %>%
    dplyr::select(Driven, Boated, Ferried, Hiked, Paddleboarded)
  
  sf::st_geometry(traveled) <- NULL
  
  traveled <- t(traveled) %>%
    as.data.frame() %>%
    dplyr::filter(!is.na(`1`)) %>%
    dplyr::mutate(Metric = paste0('Miles ', rownames(.))) %>%
    dplyr::rename(Measure = `1`) %>%
    dplyr::mutate(Measure = as.character(Measure))
  
  lon <- round(unlist(tbl$geometry)[1], 4)
  lat <- round(unlist(tbl$geometry)[2], 4)
  
  tblx <- data.frame(Metric = c('Date', 'Weather', 'Lat', 'Lon'),
                     Measure = c(as.character(tbl$Date), as.character(tbl$Weather), 
                                 as.character(lat), as.character(lon))) %>%
    dplyr::bind_rows(., traveled)
  
  rownames(tblx) <- NULL
  
  tableGrob(tblx, rows = NULL, theme = tt2)
  
}


plotDayGeos <- function(daygeos_,
                        map_service = 'osm',
                        map_type = 'terrain',
                        legend_pos = 'none',
                        line_size = 2,
                        line_colors = c('purple', 'green4'),
                        mask_color = 'white', 
                        mask_fill = 'white',
                        mask_alpha = 1,
                        stay_size = 3
){
  
  ## Set Buubox
  # bbox <- c(left = daygeos_$coords$corners[1] - scale,
  #           bottom = daygeos_$coords$corners[2] - scale/lscale,
  #           right = daygeos_$coords$corners[3] + scale,
  #           top = daygeos_$coords$corners[4] + scale/lscale)
  # 
  # coordbounds_cf <- data.frame(X = c(daygeos_$coords$corners[1] * .95,
  #                                    daygeos_$coords$corners[3] * 1.05),
  #                              Y = c(daygeos_$coords$corners[2] * .95,
  #                                    daygeos_$coords$corners[4] * 1.05))
  # 
  # 
  # coordbounds_cf <- data.frame(X = c(daygeos_$coords$corners[1] * .95,
  #                                    daygeos_$coords$corners[3] * 1.05),
  #                              Y = c(daygeos_$coords$corners[2] * .95,
  #                                    daygeos_$coords$corners[4] * 1.05))
  # 
  # bbox <- coordbounds_cf %>% 
  #   sf::st_as_sf(coords = c("X", "Y"), 
  #                crs = 3857) #%>% 
  #   #sf::st_bbox() %>% 
  #   #sf::st_as_sfc()
  # 
  # bbox = daygeos_$mask %>%
  #   sf::st_set_crs(., 3857)
  # 
  ## Grab basemap
  #basemap <- ggmap::get_map(bbox, zoom=zoom, maptype = map_type)
  basemap <- basemaps::basemap_ggplot(daygeos_$bbox%>%
                                        sf::st_transform(., 3857),
                                     map_service = map_service,
                                     map_type = map_type)
  
  # Plot
  basemap +  
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, -1, -1), 'lines'),
          legend.position = legend_pos) +
    scale_color_manual(name = '', values = line_colors) + 
    xlab('') +
    ylab('') +
    geom_sf(data = daygeos_$lines %>%
              sf::st_transform(., 3857), 
            aes(color = mode), size = line_size, inherit.aes = FALSE) +
    geom_sf(data = daygeos_$mask %>%
              sf::st_transform(., 3857), 
            color = mask_color, 
            fill = mask_fill, 
            alpha = mask_alpha,
            inherit.aes=FALSE)  +
    geom_sf(data = daygeos_$stay%>%
              sf::st_transform(., 3857),
            color = 'red', 
            shape = 16,
            size = stay_size,
            inherit.aes = FALSE)
}



createDayGeometry <- function(day_filter,
                              lines_geos,
                              stays_sf = stays_sf,
                              buff_dist = 1000,
                              ...){
  
  ## Combine lines
  for (i in 1:length(lines_geos)){
    day_sf <- lines_geos[[i]] %>%
      dplyr::filter(day == day_filter)
    if (i == 1){
      lines_sf <- day_sf
    } else {
      lines_sf <- lines_sf %>%
        dplyr::bind_rows(day_sf)
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
      sf::st_set_crs(., 4269)
  }
  
  ## Create Mask
  coords_cf <- data.frame(corners=as.vector(sf::st_bbox(buff_sf)))
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
    dplyr::filter(NightStart <= day_filter & NightEnd > day_filter)
  
  return(
    list(bbox = bounding_sf,
         lines = lines_sf,
         mask = mask_sf,
         stay = stay_sf,
         coords = coords_cf))
}



flightPath <- function(airports, air_sf){
  
  path_name <- paste0(airports[1], "_", airports[2])
  paths_sf <- air_sf[air_sf$Code %in% airports, c('X','Y')]
  
  path_sf <- geosphere::gcIntermediate(as.double(paths_sf[1, ])[1:2],  
                                       as.double(paths_sf[2, ])[1:2], 
                                       n=50, 
                                       addStartEnd=TRUE, 
                                       breakAtDateLine=T) %>%
    as.data.frame()
  path_sf$route = path_name
  path_sf
}


