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
  path_configs <- list(track = file.path(file_path, 'events'),
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
                        z_scale = 10,
                         fov = 0, 
                         theta = 315, 
                         zoom = .65, 
                         phi = 37, 
                         window_size = c(1000, 800),
                         antialias = TRUE,
                         tmap_base = leaflet::providers$CartoDB.DarkMatter )
    
### Load shared data -----------------------------------------------------------  
  
   elev_index_sf <- sf::st_read(file.path(data_path, 'base', 'elevation', 
                                          'dem_index.shp')) 
     
### RACE -- Cutthroat ----------------------------------------------------------

   track <- 'Cutthroat Classic'
   
   create_rs_plot(track_name = track,
                  elev_index = elev_index_sf,
                  path_configs = path_configs,
                  track_configs = track_configs,
                  plot_configs = plot_configs,
                  overwrite = TRUE)
  
### RACE -- Lord Hill -----------------------------------------------
  
  track <- 'Lord Hill'
   
  create_rs_plot(track_name = track,
                  elev_index = elev_index_sf,
                  path_configs = path_configs,
                  track_configs = track_configs,
                  plot_configs = plot_configs,
                  overwrite = TRUE) 
   
### RACE -- Mt Constitution Half -----------------------------------------------
  
  track <- 'Mount Constitution Half'
  
  create_rs_plot(track_name = track,
                 elev_index = elev_index_sf,
                 path_configs = path_configs,
                 track_configs = track_configs,
                 plot_configs = plot_configs,
                 overwrite = TRUE) 
  
### RACE -- Big Foot 20 -----------------------------------------------
  
  track <- 'Bigfoot 20'
  
  create_rs_plot(track_name = track,
                 elev_index = elev_index_sf,
                 path_configs = path_configs,
                 track_configs = track_configs,
                 plot_configs = plot_configs,
                 overwrite = TRUE)    
  
### EXTRA Code -----------------------------------------------------------------   
   
if (F){
  
  library(MetBrewer)
  # Specify the palette name in its own variable so that
  # we can reference it easily later.
  pal <- "Demuth"
  colors <- met.brewer(pal)
  
  https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/

  https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/

  # # Create some simple elevation plots
  # elev_df <- elev_df %>%
  #   dplyr::mutate(order = 1:nrow(elev_df))
  # 
  # ggplot(elev_df,
  #        aes(x = order, y = elevation)) +
  #   geom_line()
  # 
  
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
  
  
}   
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~