#***************************************************************************************************
#
#  Functions to handle elevation data
#
#***************************************************************************************************


downloadDEM <- function(x_df){
 
  dem_dir <- "~/dropbox/andy/data/base/elevation/dem/"
  grd_dir <- "~/dropbox/andy/data/base/elevation/grd/"
  
  if (file.exists(file.path(dem_dir, paste0(x_df$name, '.dem')))){
    cat('\nReading in:', x_df$name, '\n')
    xdem <- raster::raster(file.path(dem_dir, paste0(x_df$name, '.dem'))) %>%
      raster::projectRaster(., crs=4326)
    
  } else {
  
    cat('\nDownloading:', x_df$name, '\n')
    
    ## File Path
    download_path <- paste0("https://gis.ess.washington.edu/data/raster/",
                            "tenmeter/byquad/",
                            x_df$file)
    temp <- tempfile()
    download.file(download_path, temp)
    
    exdir = file.path(dem_dir, '_temp')
    unzip(zipfile = temp, exdir = exdir)
    zfile <- file.path(exdir, list.files(exdir))
    file.rename(zfile,
                file.path(exdir, paste0(x_df$name, '.dem')))
    file.copy(from = file.path(exdir, list.files(exdir)),
              to = file.path(dem_dir, paste0(x_df$name, '.dem')))
    file.remove(file.path(exdir, list.files(exdir)))
    
    # Convert to GRD
    xdem <- raster::raster(file.path(dem_dir, paste0(x_df$name, '.dem'))) %>%
      raster::projectRaster(., crs=4326)
    
  }
   
  if(x_df$units == 'feet'){
    cat('Converting feet to meters  \n')
    xdem <- xdem / 3.2808399  
  } 
  
  # Write Out to GRD
  writeRaster(xdem, file.path(grd_dir, paste0(x_df$name, '.grd')),
              overwrite = TRUE)  
  
  data.frame(name = x_df$name, 
             xmin = xdem@extent[1],
             xmax = xdem@extent[2],
             ymin = xdem@extent[3],
             ymax = xdem@extent[4])
}

extractDEMInfo <- function(x){
  
  # Prep Text
  dtext <- x[1]
  dtext <- gsub('\\t', 'xxxx', dtext)
  
  # Split
  ss <- strsplit(dtext, '<TD>')[[1]]
  sts <- strsplit(ss[2],"xxxx")[[1]]
  stts <- strsplit(sts, '[/]')[[1]][2]
  
  # Get Location
  xfile <- strsplit(sts[1],"=")[[1]][2]
  xfile <- substr(xfile, 1, nchar(xfile) - 1)
  
  #return
  data.frame(name = ss[3],
             code = sts[2],
             dem = strsplit(stts, '[.]')[[1]][1],
             file = xfile,
             units = ifelse(ss[5] == "D", 'meters', 'feet'))
}

lat_long_to_sf_polygon <- function(lat, long) {
  # Check for valid input
  if (length(lat) != length(long)) {
    stop("lat and long must be of equal length")
  }
  
  # Ensure the polygon is closed by repeating the first point
  lat <- c(lat, lat[1])
  long <- c(long, long[1])
  
  # Create a matrix of coordinates
  coords <- cbind(long, lat)
  
  # Create a polygon from the coordinates
  polygon <- st_polygon(list(coords))
  
  # Create an sf object with the polygon
  sf_polygon <- st_sfc(polygon, crs = 4326)  # Specify the coordinate reference system (WGS84)
  
  return(sf_polygon)
}

explodeLL <- function(x_df){
  data.frame(name = x_df$name,
             X = c(x_df$xmin, x_df$xmin, x_df$xmax, x_df$xmax, x_df$xmin),
             Y = c(x_df$ymin, x_df$ymax, x_df$ymax, x_df$ymin, x_df$ymin))
}

getElev <- function(x_sf){ 
  
  require(elevatr)
  require(sf)
  
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


