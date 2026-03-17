#***************************************************************************************************
#
#  Prepare Data For Running Maps and GAIA
#
#***************************************************************************************************

### Setup ----------------------------------------------------------------------

  ## Load libraries
  library(sf)
  library(geosphere)
  library(tidyverse)

  ## Source Custom Functions
  source(file.path(getwd(), "functions/mapping_functions.R"))

  ## Set Paths

  data_path <- '~/dropbox/andy/data/where/america'
  output_path <- '~/dropbox/andy/data/projects/running_maps/'

### Load Data ------------------------------------------------------------------  
  
  ## Load Running Shapefile
  running_sf <- sf::st_read(file.path(data_path, 'running.shp'),
                            quiet=TRUE) %>%
    sf::st_transform(4269) 

  ## Split into trail/road 

  trailruns_sf <- running_sf %>%
    dplyr::filter(!is.na(type)) %>%
    dplyr::filter(type == 'trail') 

  roadruns_sf <- running_sf %>%
    dplyr::filter(!is.na(type)) %>%
    dplyr::filter(type != 'trail')

### Write to project location --------------------------------------------------
  
  sf::st_write(trailruns_sf, 
               file.path(output_path, 'trail_runs.shp'), 
               append = FALSE)
  
  sf::st_write(roadruns_sf, 
               file.path(output_path, 'road_runs.shp'), 
               append = FALSE)

################################################################################
### TEST, CUTTING UP FOR GAIA

if(F){    
 
  # Function to convert enclosed lines to polygons
  # Works with sf LINESTRING objects that form closed loops
  
  library(sf)
  library(dplyr)
  
  
  
  lines_to_polygons <- function(lines_sf) {
    #' Convert enclosed LINESTRING geometries to POLYGON geometries
    #' 
    #' @param lines_sf An sf object with LINESTRING geometries
    #' @return An sf object with POLYGON geometries for closed lines
    #' 
    #' @details
    #' This function:
    #' 1. Identifies which lines are closed (start and end points are the same)
    #' 2. Converts closed lines to polygons
    #' 3. Optionally unions overlapping lines into polygons
    #' 4. Returns an sf object with polygon geometries
    
    # Check input is sf object
    if (!inherits(lines_sf, "sf")) {
      stop("Input must be an sf object")
    }
    
    # Get geometry type
    geom_type <- st_geometry_type(lines_sf, by_geometry = FALSE)
    
    if (!geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
      stop("Input must contain LINESTRING or MULTILINESTRING geometries")
    }
    
    # Store original CRS
    original_crs <- st_crs(lines_sf)
    
    # Function to check if a line is closed
    is_closed <- function(line) {
      coords <- st_coordinates(line)
      if (nrow(coords) < 3) return(FALSE)  # Need at least 3 points for a polygon
      
      # Check if first and last points are the same
      first <- coords[1, c("X", "Y")]
      last <- coords[nrow(coords), c("X", "Y")]
      
      return(all(first == last))
    }
    
    # Function to convert a single closed line to polygon
    line_to_polygon <- function(line_geom) {
      if (is_closed(line_geom)) {
        # Convert LINESTRING to POLYGON
        coords <- st_coordinates(line_geom)
        poly <- st_polygon(list(coords[, c("X", "Y")]))
        return(poly)
      } else {
        return(NULL)
      }
    }
    
    # Process each geometry
    polygons_list <- list()
    attributes_list <- list()
    
    for (i in 1:nrow(lines_sf)) {
      geom <- st_geometry(lines_sf[i, ])[[1]]
      
      # Handle MULTILINESTRING
      if (st_geometry_type(geom) == "MULTILINESTRING") {
        # Process each line in the multilinestring
        for (j in 1:length(geom)) {
          poly <- line_to_polygon(geom[j])
          if (!is.null(poly)) {
            polygons_list <- append(polygons_list, list(poly))
            # Preserve attributes
            attributes_list <- append(attributes_list, list(st_drop_geometry(lines_sf[i, ])))
          }
        }
      } else {
        # Single LINESTRING
        poly <- line_to_polygon(geom)
        if (!is.null(poly)) {
          polygons_list <- append(polygons_list, list(poly))
          attributes_list <- append(attributes_list, list(st_drop_geometry(lines_sf[i, ])))
        }
      }
    }
    
    # Check if any polygons were created
    if (length(polygons_list) == 0) {
      warning("No closed lines found. Returning empty sf object.")
      return(st_sf(geometry = st_sfc(crs = original_crs)))
    }
    
    # Create sf object with polygons
    polygons_sfc <- st_sfc(polygons_list, crs = original_crs)
    
    # Combine attributes
    if (length(attributes_list) > 0) {
      attributes_df <- bind_rows(attributes_list)
      polygons_sf <- st_sf(attributes_df, geometry = polygons_sfc)
    } else {
      polygons_sf <- st_sf(geometry = polygons_sfc)
    }
    
    return(polygons_sf)
  }
  
  # Alternative function: Polygonize with noding (for complex cases)
  # This works even if lines aren't perfectly closed but form enclosed areas
  lines_to_polygons_advanced <- function(lines_sf, min_area = 0) {
    #' Advanced polygonization using st_polygonize
    #' Works with lines that intersect to form enclosed areas
    #' 
    #' @param lines_sf An sf object with LINESTRING geometries
    #' @param min_area Minimum area threshold (in map units) to filter small polygons
    #' @return An sf object with POLYGON geometries
    
    if (!inherits(lines_sf, "sf")) {
      stop("Input must be an sf object")
    }
    
    # Store original CRS
    original_crs <- st_crs(lines_sf)
    
    # Combine all lines into a single geometry collection
    all_lines <- st_union(lines_sf)
    
    # Use st_polygonize to create polygons from the line network
    polygons <- st_polygonize(all_lines)
    
    # Convert to individual polygons
    if (length(polygons) > 0) {
      # Extract individual polygons
      poly_list <- st_collection_extract(polygons, "POLYGON")
      
      # Convert to sf object
      polygons_sf <- st_sf(geometry = poly_list, crs = original_crs)
      
      # Add polygon ID
      polygons_sf$polygon_id <- 1:nrow(polygons_sf)
      
      # Calculate area
      polygons_sf$area <- st_area(polygons_sf)
      
      # Filter by minimum area if specified
      if (min_area > 0) {
        polygons_sf <- polygons_sf[polygons_sf$area >= min_area, ]
      }
      
      return(polygons_sf)
    } else {
      warning("No polygons could be created from the lines.")
      return(st_sf(geometry = st_sfc(crs = original_crs)))
    }
  }
  
  
  # Helper function: Close open lines (force closure)
  close_lines <- function(lines_sf, tolerance = 0.001) {
    #' Close nearly-closed lines by snapping endpoints
    #' 
    #' @param lines_sf An sf object with LINESTRING geometries
    #' @param tolerance Distance tolerance for snapping (in map units)
    #' @return An sf object with closed LINESTRING geometries
    
    close_single_line <- function(line_geom, tol) {
      coords <- st_coordinates(line_geom)
      
      if (nrow(coords) < 2) return(line_geom)
      
      first <- coords[1, c("X", "Y")]
      last <- coords[nrow(coords), c("X", "Y")]
      
      # Calculate distance between endpoints
      dist <- sqrt(sum((first - last)^2))
      
      # If endpoints are close but not identical, close the line
      if (dist > 0 && dist <= tol) {
        coords[nrow(coords), c("X", "Y")] <- first
      }
      
      return(st_linestring(coords[, c("X", "Y")]))
    }
    
    # Apply to all geometries
    closed_geoms <- lapply(st_geometry(lines_sf), close_single_line, tol = tolerance)
    
    # Create new sf object
    closed_sf <- st_sf(
      st_drop_geometry(lines_sf),
      geometry = st_sfc(closed_geoms, crs = st_crs(lines_sf))
    )
    
    return(closed_sf)
  }
  
  
  # Example usage demonstration
  example_usage <- function() {
    cat("Example usage:\n\n")
    
    cat("# Basic usage for perfectly closed lines:\n")
    cat("polygons <- lines_to_polygons(my_lines_sf)\n\n")
    
    cat("# Advanced usage for complex line networks:\n")
    cat("polygons <- lines_to_polygons_advanced(my_lines_sf, min_area = 100)\n\n")
    
    cat("# If lines are almost closed, close them first:\n")
    cat("closed_lines <- close_lines(my_lines_sf, tolerance = 0.01)\n")
    cat("polygons <- lines_to_polygons(closed_lines)\n\n")
    
    cat("# Complete workflow:\n")
    cat("# 1. Close nearly-closed lines\n")
    cat("closed_lines <- close_lines(my_lines_sf, tolerance = 0.01)\n\n")
    cat("# 2. Convert to polygons\n")
    cat("polygons <- lines_to_polygons(closed_lines)\n\n")
    cat("# 3. Or use advanced method for line networks\n")
    cat("polygons <- lines_to_polygons_advanced(my_lines_sf, min_area = 10)\n")
  }
  
  # Print example usage
  example_usage()
  
  aa <- lines_to_polygons_advanced(running_sf) 
  tmap_mode("view")
  tm_shape(aa) + tm_polygons(fill = 'red', fill.alpha=.4) + 
    tm_shape(running_sf) + tm_lines(col='purple')
  
 #      
 # trail_coords <- sf::st_coordinates(trailruns_sf)
 # 
 # trail_cuts <- floor(nrow(trail_coords)/1000) + 1
# 
# a <- 1
# b <- 1
# 
# for(i in 1:trail_cuts){
#   b <- trail_coords[i*1000,4]-1
#   fil <- paste0('trail_', i, '.kml')
#   sf::st_write(trailruns_sf$geometry[a:b], paste0('~/dropbox/andy/data/where/gaia/', fil),
#                append=FALSE)
#   a <- b+1
# }
# 
# 
# head(h)
 
 
}