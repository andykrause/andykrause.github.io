#***************************************************************************************************
#
#  Functions to create maps
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
                        line_size = 1.5,
                        line_colors = c('red', 'green4'),
                        mask_color = 'white', 
                        mask_fill = 'white',
                        mask_alpha = 1,
                        stay_size = 3
){

  #origin <- daygeos_$stay
  
  basemap <- basemaps::basemap_ggplot(daygeos_$bbox%>%
                                        sf::st_transform(., 3857),
                                      map_service = map_service,
                                      map_type = map_type)
  

    
  # Plot
  xmap <- basemap +  
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
            inherit.aes=FALSE) +
    coord_sf(expand = F)
  
  if (!is.null(daygeos_$stay)){
    origin_sf <- daygeos_$stay[1, ]
    dest_sf <- daygeos_$stay[-1, ]
    
    xmap <- xmap + 
      geom_sf(data = origin_sf %>%
              sf::st_transform(., 3857),
            color = 'gray60',
            shape = 16,
            size = stay_size,
            inherit.aes = FALSE) +
      geom_sf(data = dest_sf %>%
              sf::st_transform(., 3857),
            color = 'black',
            shape = 18,
            size = stay_size,
            inherit.aes = FALSE) 
  }
  xmap
    
}



createDayGeometry <- function(day_nbr,
                              line_geos,
                              stays_sf,
                              buff_dist = 1000,
                              crs = 4269,
                              ...){
  
  cat('Building data for blog #: ', day_nbr, '\n')
  
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
  
  # If not lines on that day
  if(nrow(lines_sf) == 0) return(NULL)
  
  ## Create Buffer
  buff_sf <- sf::st_buffer(lines_sf, dist = buff_dist) %>%
    sf::st_make_valid() %>%
    sf::st_union() %>%
    sf::st_make_valid() 
  
  # Ensure Buffer is valid SF object
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
    dplyr::filter(NightStart <= day_nbr & NightEnd >= day_nbr) %>%
    dplyr::arrange(NightEnd) %>%
    dplyr::mutate(NightOrder = 1:dplyr::n(),
                  StayColor = ifelse(NightOrder == 1, 'gray20', 'black'))
  
  return(
    list(entry = day_nbr,
         bbox = bounding_sf,
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
 
createTrackData <- function(track_name,
                            shp_path){
  
  track_name_x <- tolower(gsub(' ', '_', track_name))
  
  ## Load Shapefile
  cat('\n Loading track for: ', track_name, '\n')
  track_sf <- sf::st_read(file.path(shp_path, paste0(track_name_x, '.shp')),
                          quiet = TRUE) %>%
    sf::st_transform(4326)
  
  # Get Elevation Data
  if (!file.exists(file.path(shp_path, 
                             paste0(track_name_x, '_elevation.shp')))){
    cat('\n Downloading elevation data\n')
    track_esf <- getElev(track_sf) %>%
      sf::st_transform(4326)
    
    sf::st_write(track_esf,
                 file.path(shp_path, 
                           paste0(track_name_x, '_elevation.shp')),
                 append = FALSE,
                 quiet = TRUE)
    
  } else {
    
    cat('\n Loading elevation data\n')
    
    track_esf <- sf::st_read(file.path(shp_path, 
                                       paste0(track_name_x, 
                                              '_elevation.shp')),
                             quiet = TRUE) %>%
      sf::st_transform(4326)
    
  }
  
  list(bbox = sf::st_bbox(track_sf),
       track = track_sf,
       elevation = track_esf)
  
}


createTopoData <- function(track_obj,
                           elev_index_sf,
                           elev_path,
                           texture,
                           track_buffer = .01,
                           water_col = 'desert',
                           ray_z = 3,
                           shadow_z = 0.5, 
                           custom_bbox = NULL){   
  
   if (is.null(custom_bbox)){
     bbox_poly <- bbox_to_poly(track_obj$track,
                               buffer = track_buffer)
   } else {
     bbox_poly <- custom_bbox
   }
  
  ## Identify the DEMs to load
  id <- sf::st_intersects(elev_index_sf, bbox_poly)
  idx <- which(lapply(id, length) > 0)
  idx_df <- elev_index_sf[idx, ]
  
  ## Load DEM and transform
  topo_ <- list()
  for (i in 1:nrow(idx_df)){
    cat('\n Loading topography for: ', idx_df$name[i], '\n')
    topo_[[i]] <- raster::raster(file.path(elev_path, 
                                           paste0(idx_df$name[i], '.grd'))) %>%
      raster::projectRaster(., crs=4326)
  }
  
  # Combine 
  if (length(topo_) == 1) {
    topo_elev <- topo_[[1]]
  } else {
    topo_elev <- topo_[[1]]
    for (k in 2:length(topo_)){
      topo_elev <- alignRasters(topo_elev, topo_[[k]])
    }
  }
  
  topo_elev <- raster::crop(topo_elev, bbox_poly)
  
  # Create elevation matrix
  topo_mat <- rayshader::raster_to_matrix(topo_elev)
  
  # Convert to rayshader matrix
  cat('\n Converting to ray-shaded DEM \n')
  topo_mat %>%
    sphere_shade(texture = texture) %>%
    add_water(detect_water(topo_mat), color = water_col) %>%
    add_shadow(ray_shade(topo_mat, zscale = ray_z), shadow_z) %>%
    add_shadow(ambient_shade(topo_mat), 0) ->
    topo_rsm
  
  
  list(bbox = bbox_poly,
       elev = topo_elev,
       mat = topo_mat,
       rsm = topo_rsm)
  
}    

bbox_to_poly <- function(sf_obj,
                         buffer = NULL){
  
  bbox_poly <- as(
    extent(sf::st_bbox(sf_obj)), 'SpatialPolygons') 
  crs(bbox_poly) <- crs(sf_obj)
  bbox_poly <- bbox_poly %>% sf::st_as_sf()
  
  if (!is.null(buffer)){
    bbox_poly <- bbox_poly %>% sf::st_buffer(., buffer)
  } 
  
  bbox_poly
}


alignRasters <- function(rast_1, rast_2, ...){

  # #template is an empty raster that has the projected extent of r2 but is 
  # aligned with r1 (i.e. same resolution, origin, and crs of r1)
  
  blank_template <- raster::projectRaster(from = rast_2, to = rast_1, 
                                          alignOnly = TRUE)
  aligned_rast_2 <- raster::projectRaster(from = rast_2, to = blank_template)
  merged_rasters <- raster::merge(rast_1, aligned_rast_2)
  
  return(merged_rasters)
  
  # If any overlap, take the mean
  #mosaic(rast_1)
  # r2_aligned<- projectRaster(from = ydem, to= template)
  # r_merged<- merge(xdem,r2_aligned) 
  # r_merged2<- mosaic(xdem,r2_aligned, fun=mean, na.rm=TRUE)
}


if (F){
  g <- sf::st_bbox(track_obj$track)
  b <- as(extent(g), 'SpatialPolygons')
  crs(b) <- crs(track_obj$track)
  bb <- st_as_sf(b) # convert polygons to 'sf' object
   plot(rb)  
  topo_mat <- rayshader::raster_to_matrix(rb)
  
  # Convert to rayshader matrix
  cat('\n Converting to ray-shaded DEM \n')
  topo_mat %>%
    sphere_shade(texture = texture) %>%
    add_water(detect_water(topo_mat), color = water_col) %>%
    add_shadow(ray_shade(topo_mat, zscale = ray_z), shadow_z) %>%
    add_shadow(ambient_shade(topo_mat), 0) ->
    topo_rsm
  
  topo_rsm %>%
    plot_3d(topo_obj$mat, 
            zscale = plot_z, 
            fov = 0, 
            theta = 315, 
            zoom = .65, 
            phi = 37, 
            windowsize = c(1000, 800))
  
}  
  
  
  
  
  
  
  
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
