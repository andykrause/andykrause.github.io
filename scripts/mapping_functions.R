

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
plotPark <- function(park_code,
                     zoom = 11,
                     scale = .2,
                     lscale = 1.6,
                     map_type = 'toner-background',
                     park_color = 'green4',
                     circle = FALSE,
                     cscale = 2,
                     cpscale = 80,
                     spin = 1){
  
  park_df <- parks_df[which(parks_df$ParkCode == park_code)[1], ]
  park_sf <- parks_poly_sf[which(parks_poly_sf$ParkCode == park_code)[1], ]
  
  bbox <- c(left = park_df$Longitude - scale,
            bottom = park_df$Latitude - scale/lscale,
            right = park_df$Longitude + scale,
            top = park_df$Latitude + scale/lscale)
  
  basemap <- ggmap::get_stamenmap(bbox, zoom=zoom, maptype = map_type)
  
  base_plot <- 
    ggmap(basemap) + 
    geom_sf(data = park_sf, aes(fill=ParkName), inherit.aes = FALSE, color = park_color) +
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
