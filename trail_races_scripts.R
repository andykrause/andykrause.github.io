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
  #source(file.path(getwd(), 'functions', '_functions.R'))

  ## Set Paths
  data_path <- '~/dropbox/andy/data/'
  elev_path <- file.path(data_path, 'base', 'elevation', 'grd')
  event_path <- file.path(data_path, 'where', 'events')
  image_path <- file.path(getwd(), 'images')

## Configs & Parameters --------------------------------------------------------
  
   # TMap basemaps
    basemap <- leaflet::providers$CartoDB.DarkMatter 
  
   # Rayshader Textures
   #texture <- create_texture("darkgreen", "green4", "gold", "brown", "gray70")
   #texture <- create_texture("#fff673","#55967a","#8fb28a","#55967a","#cfe0a9") 
   #texture <- create_texture("#55967a","#8fb28a","#55967a","#cfe0a9","#fff673") 
   texture <- rayshader::create_texture("#55967a","#8fb28a","brown4",
                                        "#cfe0a9","gray70") 
  
   # Other Rayshader configs
   path_color <- 'white'
   plot_z <- 10 
   path_elev_multiplier <- 1.02 

   elp <- "~/dropbox/andy/data/base/elevation/"
  
   elev_index_sf <- sf::st_read(file.path(elp, 'dem_index.shp')) 
     
### RACE -- Cutthroat ----------------------------------------------------------

   track <- 'Cutthroat Classic'
   trp <- file.path(file_path, 'events')
   
   plot_name <- paste0(tolower(gsub(' ', '_', track)), '.png')
   
   ## Calculate Race Data
   if (overwrite == TRUE | 
       !file.exists(file.path(image_path, plot_name))){
     
     # Create Track Data
     track_obj <- createTrackData(track_name = track,
                                  shp_path = trp)
     
     
     # Create Race Data  
     topo_obj <- createTopoData(track_obj = track_obj,
                                elev_index_sf = elev_index_sf,
                                elev_path = elev_path,
                                texture = texture)
     
     ## Make 3Plot
     
     topo_obj$rsm %>%
       plot_3d(topo_obj$mat, 
               zscale = plot_z, 
               fov = 0, 
               theta = 315, 
               zoom = .65, 
               phi = 37, 
               windowsize = c(1000, 800))
     
     # Add Path
     render_path(extent = st_bbox(topo_obj$elev),
                 lat = unlist(track_obj$elevation$lat),
                 long = unlist(track_obj$elevation$lon),
                 altitude = unlist(track_obj$elevation$elevation) * 
                   path_elev_multiplier,
                 zscale = plot_z,
                 color = path_color,
                 antialias = TRUE)
     
     # Export the image
     rgl::rgl.snapshot(file.path(image_path, plot_name), fmt = 'png')
     
   }   
   
   
  
### IN PROGRESS
  
  stop()
  
  
### RACE -- Lord Hill -----------------------------------------------
  
  track <- 'Lord Hill'
  trp <- file.path(file_path, 'events')
  
  plot_name <- paste0(tolower(gsub(' ', '_', track)), '.png')
  
  ## Calculate Race Data
  if (overwrite == TRUE | 
      !file.exists(file.path(image_path, plot_name))){
    
    # Create Track Data
    track_obj <- createTrackData(track_name = track,
                                 shp_path = trp)
    
    
    # Create Race Data  
    topo_obj <- createTopoData(track_obj = track_obj,
                               elev_index_sf = elev_index_sf,
                               elev_path = elev_path,
                               texture = texture)
    
    ## Make 3Plot
    
    topo_obj$rsm %>%
      plot_3d(topo_obj$mat, 
              zscale = plot_z, 
              fov = 0, 
              theta = 315, 
              zoom = .65, 
              phi = 37, 
              windowsize = c(1000, 800))
    
    # Add Path
    render_path(extent = st_bbox(topo_obj$elev),
                lat = unlist(track_obj$elevation$lat),
                long = unlist(track_obj$elevation$lon),
                altitude = unlist(track_obj$elevation$elevation) * 
                  path_elev_multiplier,
                zscale = plot_z,
                color = path_color,
                antialias = TRUE)
    
    # Export the image
    rgl::rgl.snapshot(file.path(image_path, plot_name), fmt = 'png')
    
  }   
  
  
  
  
  
  # ## Set Paths
  # 
  # rsp <- file.path(file_path, 'events', 'lord_hill.shp')
  # dmp <- "~/dropbox/andy/data/base/elevation/maltby.dem"
  # 
  # ## Calculate Race Data
  # if (overwrite == TRUE | 
  #     !file.exists(file.path(image_path, 'lord_hill_plot.png'))){
  #   
  #   # Create Race Data  
  #   lh_race <- createRaceData(race_shp_path = rsp,
  #                              dem_path = dmp,
  #                              texture = texture)
  #   
  #   ## Make 3Plot
  #   
  #   lh_race$rsm %>%
  #     plot_3d(lh_race$mat, 
  #             zscale = plot_z, 
  #             fov = 0, 
  #             theta = 315, 
  #             zoom = .65, 
  #             phi = 37, 
  #             windowsize = c(1000, 800))
  #   
  #   # Add Path
  #   render_path(extent = st_bbox(lh_race$dem),
  #               lat = unlist(lh_race$edf$lat),
  #               long = unlist(lh_race$edf$lon),
  #               altitude = unlist(lh_race$edf$elevation) * path_elev_multiplier,
  #               zscale = plot_z,
  #               color = path_color,
  #               antialias = TRUE)
  #   
  #   # Export the image
  #   rgl::rgl.snapshot(file.path(image_path, 'lord_hill_plot.png'), 
  #                     fmt = 'png')
  #   
  # }   
  
  
  
### RACE -- Mt Constitution Half -----------------------------------------------
  
  track <- 'Mount Constitution Half'
  trp <- file.path(file_path, 'events')
  
  plot_name <- paste0(tolower(gsub(' ', '_', track)), '.png')
  
  ## Calculate Race Data
  if (overwrite == TRUE | 
      !file.exists(file.path(image_path, plot_name))){
    
    # Create Track Data
    track_obj <- createTrackData(track_name = track,
                                 shp_path = trp)
    
    
    # Create Race Data  
    topo_obj <- createTopoData(track_obj = track_obj,
                               elev_index_sf = elev_index_sf,
                               elev_path = elev_path,
                               texture = texture)
    
    ## Make 3Plot
    
    topo_obj$rsm %>%
      plot_3d(topo_obj$mat, 
              zscale = plot_z, 
              fov = 0, 
              theta = 315, 
              zoom = .65, 
              phi = 37, 
              windowsize = c(1000, 800))
    
    # Add Path
    render_path(extent = st_bbox(topo_obj$elev),
                lat = unlist(track_obj$elevation$lat),
                long = unlist(track_obj$elevation$lon),
                altitude = unlist(track_obj$elevation$elevation) * 
                  path_elev_multiplier,
                zscale = plot_z,
                color = path_color,
                antialias = TRUE)
    
    # Export the image
    rgl::rgl.snapshot(file.path(image_path, plot_name), fmt = 'png')
    
  }   
  
  
  
  ## Set Paths
  
  rsp <- file.path(file_path, 'events', 'mount_constitution_half.shp')
  dmp <- "~/dropbox/andy/data/base/elevation/mount constitution.dem"
  
  ## Calculate Race Data
  if (overwrite == TRUE | 
      !file.exists(file.path(image_path, 'mount_constitution_plot.png'))){
    
    # Create Race Data  
    mc_race <- createRaceData(race_shp_path = rsp,
                              dem_path = dmp,
                              texture = texture)
    
    ## Make 3Plot
    
    mc_race$rsm %>%
      plot_3d(mc_race$mat, 
              zscale = plot_z, 
              fov = 0, 
              theta = 315, 
              zoom = .65, 
              phi = 37, 
              windowsize = c(1000, 800))
    
    # Add Path
    render_path(extent = st_bbox(mc_race$dem),
                lat = unlist(mc_race$edf$lat),
                long = unlist(mc_race$edf$lon),
                altitude = unlist(mc_race$edf$elevation) * path_elev_multiplier,
                zscale = plot_z,
                color = path_color,
                antialias = TRUE)
    
    # Export the image
    rgl::rgl.snapshot(file.path(image_path, 'mount_constitution_plot.png'), 
                      fmt = 'png')
    
  }   
  
  
### RACE -- Big Foot 20 -----------------------------------------------
  
  
  track <- 'Bigfoot 20'
  trp <- file.path(file_path, 'events')
  
  plot_name <- paste0(tolower(gsub(' ', '_', track)), '.png')
  
  ## Calculate Race Data
  if (overwrite == TRUE | 
      !file.exists(file.path(image_path, plot_name))){
    
    # Create Track Data
    track_obj <- createTrackData(track_name = event,
                                 shp_path = trp)
    
    
    # Create Race Data  
    topo_obj <- createTopoData(track_obj = track_obj,
                               elev_index_sf = elev_index_sf,
                               elev_path = elev_path,
                               texture = texture,
                               track_buffer = 1000)
    
  
    topo_obj$rsm %>%
      plot_3d(topo_obj$mat, 
              zscale = plot_z, 
              fov = 0, 
              theta = 315, 
              zoom = .65, 
              phi = 37, 
              windowsize = c(1000, 800))
    
    # Add Path
    render_path(extent = st_bbox(topo_obj$elev),
                lat = unlist(track_obj$elevation$lat),
                long = unlist(track_obj$elevation$lon),
                altitude = unlist(track_obj$elevation$elevation) * 
                  path_elev_multiplier,
                zscale = plot_z,
                color = path_color,
                antialias = TRUE)
  
  ## Set Paths
 
    
  }   
  
  
### EXTRA Code -----------------------------------------------------------------   
   
if (F){



  # # Create some simple elevation plots
  # elev_df <- elev_df %>%
  #   dplyr::mutate(order = 1:nrow(elev_df))
  # 
  # ggplot(elev_df,
  #        aes(x = order, y = elevation)) +
  #   geom_line()
  # 
  
  
  
}   
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~