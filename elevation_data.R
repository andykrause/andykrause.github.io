#***************************************************************************************************
#
#  Script for downloading elevation data and creating index
#
#***************************************************************************************************

 ## Setup
  library(raster)
  library(sf)
  library(dplyr)
  
  source(file.path(getwd(), 'functions', 'elevation_functions.R'))

 ## Dowloading DEM and GRI data

  # Get list of DEMs
  dem_list <- readLines(paste0('https://gis.ess.washington.edu/data/raster/',
                               'tenmeter/byquad/master.html'))

  # Remove bad rows (those without DEMs)
  dem_list <- dem_list[grepl('zip', dem_list)]

  # Extract information into a DF
  demlist_df <- purrr::map(.x = dem_list,
                       .f = extractDEMInfo) %>%
    dplyr::bind_rows()

  # Set Capture List
  dem_ <- list()

  # Got through each and download

  for(k in 1:nrow(demlist_df)){
    cat('\n:', k, '\n')
    demlist_[[k]] <- downloadDEM(demlist_df[k, ])
  }

  # Convert to an index file, and write
  dem_index_df <- dplyr::bind_rows(demlist_) 

  # Create exploded list with ffive corner points for each grid
  demcorners_ <- list()
  for (i in 1:nrow(dem_index_df)){
    demcorners_[[i]] <- explodeLL(dem_index_df[i, ])
  }
  
  # Flatten to df
  demcorners_df <- dplyr::bind_rows(demcorners_)
  
  # Create index
  gridindex_sf <- sf::st_as_sf(demcorners_df,
                               coords = c('X', 'Y')) %>%
    sf::st_set_crs(4326) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise() %>%
    dplyr::ungroup() %>%  # Just in case
    sf::st_convex_hull() %>%
    dplyr::left_join(., demlist_df, 
                     by = 'name') %>%
    dplyr::select(name, code, dem, units, file, geometry)

  # Wrtie as shapefile
  sf::st_write(gridindex_sf, '~/dropbox/andy/data/base/elevation/dem_index.shp',
               append = FALSE)
  