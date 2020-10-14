

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
    ggmap(basemap) + 
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
  
  tblx <- data.frame(Metric = c('Date', 'Weather', 'Lat', 'Long'),
                     Measure = c(as.character(tbl$Date), as.character(tbl$Weather), 
                                 as.character(lat), as.character(lon))) %>%
    dplyr::bind_rows(., traveled)
  
  rownames(tblx) <- NULL
  
  tableGrob(tblx, rows = NULL, theme = tt2)
  
}
