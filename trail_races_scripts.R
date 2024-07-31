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

  ## Set Paths

  data_path <- '~/dropbox/andy/data/where'
  event_path <- file.path(data_path, 'events')
  image_path <- file.path(getwd(), 'images')

### Custom functions -----------------------------------------------------------
  getElev <- function(x_sf){ 
      
    coords <- sf::st_coordinates(x_sf) 
    xy_sf <- sf::st_as_sf(data.frame(X = coords[ ,1],  
                                     Y = coords[ ,2],  
                                     id = coords[ ,3]),  
                                     coords = c('X', 'Y'),  
                                     crs = 4326)  %>% 
        dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                      lat = sf::st_coordinates(.)[,2]) 
          
        elevatr::get_elev_point(xy_sf, src = "epqs") 
  }
  
  createTrackData <- function(event_name,
                              shp_path){
    
    event_name_x <- tolower(gsub(' ', '_', event_name))
    
    ## Load Shapefile
    cat('\n Loading track for: ', event_name, '\n')
    track_sf <- sf::st_read(file.path(shp_path, paste0(event_name_x, '.shp')),
                            quiet = TRUE) %>%
      sf::st_transform(4326)
   
    # Get Elevation Data
    if (!file.exists(file.path(shp_path, 
                               paste0(event_name_x, '_elevation.shp')))){
      cat('\n Downloading elevation data\n')
      track_esf <- getElev(track_sf) %>%
        sf::st_transform(4326)
      
      sf::st_write(track_esf,
                   file.path(shp_path, 
                             paste0(event_name_x, '_elevation.shp')),
                   append = FALSE,
                   quiet = TRUE)
      
    } else {
      
      cat('\n Loading elevation data\n')
      
      track_esf <- sf::st_read(file.path(shp_path, 
                                            paste0(event_name_x, 
                                                   '_elevation.shp')),
                              quiet = TRUE) %>%
        sf::st_transform(4326)
      
    }
    
    list(bbox = sf::st_bbox(track_sf),
         track = track_sf,
         elevation = track_esf)
     
  }

  
    
  createRaceData <- function(race_shp_path,
                             dem_path,
                             texture,
                             water_col = 'desert',
                             ray_z = 3,
                             shadow_z = 0.5){   
    

    
    ## Load DEM and transform
    cat('\n Loading: ', dem_path, '\n')
    race_dem <- raster::raster(dem_path) %>%
      raster::projectRaster(., crs=4326)
    

    # Create elevation matrix
    race_mat <- rayshader::raster_to_matrix(race_dem)
    
    # Convert to rayshader matrix
    cat('\n Converting to ray-shaded DEM \n')
    race_mat %>%
      sphere_shade(texture = texture) %>%
      add_water(detect_water(race_mat), color = water_col) %>%
      add_shadow(ray_shade(race_mat, zscale = ray_z), shadow_z) %>%
      add_shadow(ambient_shade(race_mat), 0) ->
      race_rsm
    
    list(shp = race_sf,
         dem = race_dem,
         edf = race_edf,
         mat = race_mat,
         rsm = race_rsm)
    
  }    
  
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
  
### RACE -- Cutthroat ----------------------------------------------------------

   event <- 'Cutthroat Classic'
   
  ## Set Paths

  rsp <- file.path(file_path, 'events', 'cutthroat_classic.shp')
  dmp <- "~/dropbox/andy/data/base/elevation/washington_pass.dem"

  ## Calculate Race Data
  if (overwrite == TRUE | 
      !file.exists(file.path(image_path, 'cutthroat_plot.png'))){
  
    # Create Track Data
    track_obj <- createTrackData(event_name = event,
                                 shp_path = event_path)
    
    
    # Create Race Data  
    ct_race <- createRaceData(race_shp_path = rsp,
                              dem_path = dmp,
                              texture = texture)
  
    ## Make 3Plot
  
     ct_race$rsm %>%
       plot_3d(ct_race$mat, 
               zscale = plot_z, 
               fov = 0, 
               theta = 315, 
               zoom = .65, 
               phi = 37, 
               windowsize = c(1000, 800))
   
     # Add Path
     render_path(extent = st_bbox(ct_race$dem),
                 lat = unlist(ct_race$edf$lat),
                 long = unlist(ct_race$edf$lon),
                 altitude = unlist(ct_race$edf$elevation) * path_elev_multiplier,
                 zscale = plot_z,
                 color = path_color,
                 antialias = TRUE)
   
     # Export the image
     rgl::rgl.snapshot(file.path(image_path, 'cutthroat_plot.png'), fmt = 'png')
   
  }   
     
  
### RACE -- Lord Hill -----------------------------------------------
  
  ## Set Paths
  
  rsp <- file.path(file_path, 'events', 'lord_hill.shp')
  dmp <- "~/dropbox/andy/data/base/elevation/maltby.dem"
  
  ## Calculate Race Data
  if (overwrite == TRUE | 
      !file.exists(file.path(image_path, 'lord_hill_plot.png'))){
    
    # Create Race Data  
    lh_race <- createRaceData(race_shp_path = rsp,
                               dem_path = dmp,
                               texture = texture)
    
    ## Make 3Plot
    
    lh_race$rsm %>%
      plot_3d(lh_race$mat, 
              zscale = plot_z, 
              fov = 0, 
              theta = 315, 
              zoom = .65, 
              phi = 37, 
              windowsize = c(1000, 800))
    
    # Add Path
    render_path(extent = st_bbox(lh_race$dem),
                lat = unlist(lh_race$edf$lat),
                long = unlist(lh_race$edf$lon),
                altitude = unlist(lh_race$edf$elevation) * path_elev_multiplier,
                zscale = plot_z,
                color = path_color,
                antialias = TRUE)
    
    # Export the image
    rgl::rgl.snapshot(file.path(image_path, 'lord_hill_plot.png'), 
                      fmt = 'png')
    
  }   
  
  
  
### RACE -- Mt Constitution Half -----------------------------------------------
  
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
  
  ## Set Paths
  
  rsp <- file.path(file_path, 'events', 'bigfoot_20.shp')
  dmp <- "~/dropbox/andy/data/base/elevation/mount saint helens.grd"
  
  ## Calculate Race Data
  if (overwrite == TRUE | 
      !file.exists(file.path(image_path, 'mount_constitution_plot.png'))){
    
    # Create Race Data  
    bf_race <- createRaceData(race_shp_path = rsp,
                              dem_path = dmp,
                              texture = texture)
    
    ## Make 3Plot
   
    
    bf_race$rsm %>%
      plot_3d(bf_race$mat, 
              zscale = plot_z, 
              fov = 0, 
              theta = 315, 
              zoom = .65, 
              phi = 37, 
              windowsize = c(1000, 800))
    
    # Add Path
    render_path(extent = st_bbox(bf_race$dem),
                lat = unlist(bf_race$edf$lat),
                long = unlist(bf_race$edf$lon),
                altitude = unlist(bf_race$edf$elevation) * path_elev_multiplier,
                zscale = plot_z,
                color = path_color,
                antialias = TRUE)
    
    # Export the image
    rgl::rgl.snapshot(file.path(image_path, 'bigfoot_20_plot.png'), 
                      fmt = 'png')
    
  }   
  
  
### EXTRA Code -----------------------------------------------------------------   
   
if (F){

  # Where to get Raster DEM data  
  # https://gis.ess.washington.edu/data/raster/tenmeter/byquad/
  #https://gis.ess.washington.edu/data/raster/tenmeter/byquad/concrete/index.html
  area <- 'hoquiam'
  num <- 'f2322'
  name <- 'Goat Mountain'
  xx <- paste0("https://gis.ess.washington.edu/data/raster/tenmeter/byquad/",
               area, "/", num, ".zip")
  temp <- tempfile()
  download.file(xx,temp)
  dmp_dir <- "~/dropbox/andy/data/base/elevation/"
  
  unzip(zipfile = temp, exdir = dmp_dir)
  
  file.rename(file.path(dmp_dir, paste0(num,'.dem')),
              file.path(dmp_dir, paste0(name, '.dem')))
  
  if(grep('f', num)){
    xdem <- raster::raster(file.path(dmp_dir, paste0(name, '.dem'))) %>%
      raster::projectRaster(., crs=4326)
    xdem <- xdem / 3.2808399
    writeRaster(xdem, file.path(dmp_dir, paste0(name, '.grd')))  
  }
  
  template<- projectRaster(from = ydem, to= xdem, alignOnly=TRUE)
  #template is an empty raster that has the projected extent of r2 but is aligned with r1 (i.e. same resolution, origin, and crs of r1)
  r2_aligned<- projectRaster(from = ydem, to= template)
  r_merged<- merge(xdem,r2_aligned) 
  r_merged2<- mosaic(xdem,r2_aligned, fun=mean, na.rm=TRUE)
  
  https://gis.ess.washington.edu/data/raster/tenmeter/byquad/master.html
  
  g <- readLines('https://gis.ess.washington.edu/data/raster/tenmeter/byquad/master.html')
  
  gg <- g[8:length(g)]
  
  w <- gg[1]
  
  gsub('\\t', '#', w)
  ss<-strsplit(w, '<TD>')
  
  name <- ss[3]
  code <- strsplit(ss[2],"\\t")[2]
  
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