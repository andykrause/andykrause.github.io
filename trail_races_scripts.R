#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#.  Script for creating trail race images and data
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Setup ----------------------------------------------------------------------

 ## Global parameters

  overwrite <- FALSE # Should we overwrite the existing plots?  

 ## Load libraries
  library(sf)
  library(geosphere)
  library(tidyverse)
  library(basemaps)
  library(maps)
  library(tmap)
  library(elevatr)
  library(raster)
  library(rayshader)

  ## Source Custom Functions
  source(file.path(getwd(), "functions/mapping_functions.R"))
  source(file.path(getwd(), 'functions', 'elevation_functions.R'))

  ## Set Paths
  data_path <- '~/dropbox/andy/data/'
  elev_path <- file.path(data_path, 'base', 'elevation', 'grd')
  event_path <- file.path(data_path, 'where', 'events')
  image_path <- file.path(getwd(), 'images')

  
## Configs & Parameters --------------------------------------------------------
  
  # Set Default Path Configs
  path_configs <- list(track = file.path(event_path),
                       elev = elev_path,
                       image = image_path)
 
  # Set Default Track Configs
  track_configs <- list(buffer = 1000,
                        elev_multiplier = 1.02,
                        color = 'gray95')
  
  
  ## Set Default Plot Configs
  
   # Set Texture
   rs_texture <- rayshader::create_texture("#55967a","#8fb28a","brown4",
                                          "#cfe0a9","gray70") 
  
   # Combined COnfigs
   plot_configs <- list(texture = rs_texture,
                        shade_height = TRUE,
                        z_scale = 10,
                         fov = 0, 
                         theta = 315, 
                         zoom = .65, 
                         phi = 37, 
                         window_size = c(1000, 800),
                         antialias = TRUE,
                         tmap_base = leaflet::providers$CartoDB.DarkMatter,
                        elev_range = NULL,
                        start_end_colors = c('white', 'black'),
                        start_end_size = 10)
    
### Load shared data -----------------------------------------------------------  
  
   elev_index_sf <- sf::st_read(file.path(data_path, 'base', 'elevation', 
                                          'dem_index.shp')) 
     
### RACE -- Cutthroat ----------------------------------------------------------

   track <- 'Cutthroat Classic'
   
   # Plot Changes
   plot_configs$shade_height = TRUE
   track_configs$color = 'darkred'
   
   createRSplot(track_name = track,
                  elev_index = elev_index_sf,
                  path_configs = path_configs,
                  track_configs = track_configs,
                  plot_configs = plot_configs,
                  overwrite = TRUE)
  
### RACE -- Lord Hill -----------------------------------------------
  
  track <- 'Lord Hill'
   
  plot_configs$elev_range = c(0, 700)
   
  createRSplot(track_name = track,
                  elev_index = elev_index_sf,
                  path_configs = path_configs,
                  track_configs = track_configs,
                  plot_configs = plot_configs,
                  overwrite = TRUE) 
   
### RACE -- Mt Constitution Half -----------------------------------------------
  
  track <- 'Mount Constitution Half'
  
  plot_configs$elev_range = c(0, 1500)
  
  createRSplot(track_name = track,
                 elev_index = elev_index_sf,
                 path_configs = path_configs,
                 track_configs = track_configs,
                 plot_configs = plot_configs,
                 overwrite = TRUE) 
  
### RACE -- Big Foot 20 -----------------------------------------------
  
  track <- 'Bigfoot 20'
  track_configs$buffer = 1850  
  plot_configs$elev_range = NULL
  
  createRSplot(track_name = track,
                 elev_index = elev_index_sf,
                 path_configs = path_configs,
                 track_configs = track_configs,
                 plot_configs = plot_configs,
                 overwrite = TRUE)    
  
### EXTRA Code -----------------------------------------------------------------   
   
if (F){
  
  # Set Default Path Configs
  path_configs <- list(track = file.path(event_path),
                       elev = elev_path,
                       image = image_path)
  
  # Set Default Track Configs
  track_configs <- list(buffer = 1000,
                        elev_multiplier = 1.02,
                        color = 'gray95')
  
  
  ## Set Default Plot Configs
  
  # Set Texture
  rs_texture <- rayshader::create_texture("#55967a","#8fb28a","brown4",
                                          "#cfe0a9","gray70") 
  
  # Combined COnfigs
  plot_configs <- list(texture = rs_texture,
                       shade_height = TRUE,
                       z_scale = 10,
                       fov = 0, 
                       theta = 315, 
                       zoom = .65, 
                       phi = 37, 
                       window_size = c(1000, 800),
                       antialias = TRUE,
                       tmap_base = leaflet::providers$CartoDB.DarkMatter,
                       start_end_colors = c('white', 'black'),
                       start_end_size = 10)
  ### Sandbox lord hill example
  track_name = 'lord hill'
  
  track_obj <- createTrackData(track_name = track_name,
                               shp_path = path_configs$track)
  
  
  # Create Race Data  
  bbox_poly <- bbox_to_poly(track_obj$track,
                            buffer = 1200)
  
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
  
  cat('\n Converting to ray-shaded DEM \n')
  # if (shade_height){
    shade_obj <- topo_mat %>%
      height_shade(range = c(0,500))
  # } else {
  #   shade_obj <- topo_mat %>%
  #     sphere_shade(texture = texture)
  # }
  
  shade_obj %>%
    add_water(detect_water(topo_mat), color = 'blue') %>%
    add_shadow(ray_shade(topo_mat, zscale = 10), .5) %>%
    add_shadow(ambient_shade(topo_mat), 0) ->
    topo_rsm
                            
  ## Make base 3dPlot
  topo_rsm %>%
    plot_3d(topo_mat, 
            zscale = plot_configs$z_scale, 
            fov = plot_configs$fov, 
            theta = plot_configs$theta, 
            zoom = plot_configs$zoom, 
            phi = plot_configs$phi, 
            windowsize = plot_configs$window_size,
            background="black")
  
  ## Add track
  render_path(extent = st_bbox(topo_obj$elev),
              lat = unlist(track_obj$elevation$lat),
              long = unlist(track_obj$elevation$lon),
              altitude = unlist(track_obj$elevation$elevation) * 
                track_configs$elev_multiplier,
              zscale = plot_configs$z_scale,
              color = track_configs$color,
              antialias = plot_configs$antialias)
  
  ## Add points
  idx <- c(1, nrow(track_obj$elevation))
  
  render_points(extent = st_bbox(topo_obj$elev),
                lat = unlist(track_obj$elevation$lat[idx]),
                long = unlist(track_obj$elevation$lon[idx]),
                altitude = unlist(track_obj$elevation$elevation[idx]) * 
                  track_configs$elev_multiplier * 1.01,
                zscale = plot_configs$z_scale,
                color = plot_configs$start_end_colors,
                size = plot_configs$start_end_size)
  
  
  
  
  
  
  
  library(MetBrewer)
  # Specify the palette name in its own variable so that
  # we can reference it easily later.
  pal <- "Demuth"
  colors <- met.brewer(pal)
  # 
  # https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/
  # 
  # https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/
  # https://rallydatajunkie.com/visualising-rally-stages/introducing-3d-rayshader-models.html
  # 

  # # Create some simple elevation plots
  # elev_df <- elev_df %>%
  #   dplyr::mutate(order = 1:nrow(elev_df))
  # 
  # ggplot(elev_df,
  #        aes(x = order, y = elevation)) +
  #   geom_line()
  # 
  topo_mat %>%
  rayshader::height_shade( ) %>%
  #add_water(detect_water(topo_mat), color = water_col) %>%
  add_shadow(ray_shade(topo_mat, zscale = 10), .5) %>%
  add_shadow(ambient_shade(topo_mat), 0) %>%
  rayshader::plot_3d(., topo_mat, zscale = 10)
  #rayshader::plot_map()
  

    topo_obj$mat |>
         height_shade(texture = grDevices::colorRampPalette(colors)(256)) |>
         plot_3d(heightmap = topo_obj$mat,
                 zscale = 5) 
  
  
  # Rayshader Textures
  #texture <- create_texture("darkgreen", "green4", "gold", "brown", "gray70")
  #texture <- create_texture("#fff673","#55967a","#8fb28a","#55967a","#cfe0a9") 
  #texture <- create_texture("#55967a","#8fb28a","#55967a","#cfe0a9","#fff673") 
  # a <- MetBrewer::met.brewer(pal)[c(2,4,6,8,10)]
  # 
  # texture <- rayshader::create_texture(a[1], a[2], a[3], a[4], a[5])
  
  
    track <- 'Bigfoot 200'
    track_configs$buffer = 1850  
    plot_configs$elev_range = NULL
    
    createRSplot(track_name = track,
                 elev_index = elev_index_sf,
                 path_configs = path_configs,
                 track_configs = track_configs,
                 plot_configs = plot_configs,
                 overwrite = TRUE)  
    
}   
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~