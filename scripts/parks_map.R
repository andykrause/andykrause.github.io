#***************************************************************************************************
#
#  Script for making State Park Maps
#
#***************************************************************************************************

  ## Set Parameters
  
  reload_state <- FALSE   # Do you to reload base map from census?

  ## Load Libraries
  
  library(tidyverse)
  library(tidycensus)
  library(sf)
  #library(OpenStreetMap)
  library(ggmap)

  ## Load Custom Functions
  source(file.path(getwd(), 'scripts', 'create_park_data.R'))
  source(file.path(getwd(), 'functions', 'mapping_functions.R'))
 
  ## Load Data
  data_df <- readRDS(file.path(getwd(), 'data','created', 'data.RDS'))
  points_sf <- readRDS(file.path(getwd(), 'data','created', 'points.RDS'))
  boundaries_sf <- readRDS(file.path(getwd(), 'data','created', 'boundaries.RDS'))
  drives_sf <- readRDS(file.path(getwd(), 'data','created', 'drives.RDS'))
  hikes_sf <- readRDS(file.path(getwd(), 'data','created', 'hikes.RDS'))
  paddle_sf <- readRDS(file.path(getwd(), 'data','created', 'paddle.RDS'))
  stays_sf <- readRDS(file.path(getwd(), 'data','created', 'stays.RDS'))
  
  wa_sf <- readRDS(file.path(getwd(), 'data','created', 'county_boundaries.RDS'))
   
### State Wide Static Map --------------------------------------------------------------------------    
  
 ## Create Map
 state_map <-   
   ggplot(data = wa_sf) + 
     geom_sf(fill = 'gray90', color = 'white') + 
     theme(axis.title=element_blank(),
           axis.text=element_blank(),
           axis.ticks=element_blank(),
           panel.border = element_blank(), 
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(), 
           legend.position = 'none',
           plot.title = element_text(color="gray50", size=18, face="italic", hjust = 0.5)) + 
     geom_sf(data = points_sf, aes(color = Visited, size = Visited, shape = Visited),
             alpha = .8) + 
     scale_color_manual(values = c('gray10', 'green4')) + 
     scale_size_manual(values = c(1.1, 2.6)) + 
     scale_shape_manual(values = c(20, 20)) + 
    ggtitle('Click for an interactive version')
 
  ## Save to file
  png(filename = file.path(getwd(), 'images', 'state_map.png'), width = 780, height = 480)
    state_map
  dev.off()
 
### Individual Park Maps ---------------------------------------------------------------------------
 
 ## Set Parameters
 
  image_width <- 800
  image_height <- 800
  cp_scale <- 500 # Width of boundary to create circle
  c_scale <- 1.6  # Scale of lat to long to create square
  x_scale <- .15 # Map zoom scale (width in lon)

  ## Limit to visited
  
  visited_sf <- points_sf %>%
     dplyr::left_join(., data_df %>% dplyr::select(Abbrv, Date = Date_Visited, Order),
                      by = 'Abbrv') %>%
     dplyr::filter(Visited == 1) %>%
     dplyr::mutate(Date = as.Date(Date, '%m/%d/%y')) %>%
     dplyr::arrange(desc(Order))
  
  saveRDS(visited_sf %>%
            dplyr::left_join(., data_df %>%
                               dplyr::select(Abbrv, Driven, Boated, Ferried, Hiked, Paddleboarded),
                             by = 'Abbrv'),
          file.path(getwd(), 'data', 'created', paste0('visited.RDS')))
  
  ## Print Maps
  
  #for (k in 1:nrow(visited_sf)){
  for (k in 1:1){
      
     i_sf <- visited_sf[k,]
     cat('Drawing: ', i_sf$ParkName, '\n')
     shortname <- tolower(i_sf$ParkName)
     shortname <- gsub(' ', '', shortname)
     
     png(filename = file.path(getwd(), 'images', paste0('map_', shortname, '.png')), 
                              width = image_width, height = image_height)
       plotPark(abbr = i_sf$Abbrv,
                scale = x_scale, 
                map_type = 'toner', 
                park_color = 'red', 
                circle = TRUE, 
                cscale = c_scale, 
                cpscale = cp_scale)
       
     dev.off()
  }
  
### Create summary table ---------------------------------------------------------------------------
  
  summ_df <- 
   data.frame(
    Name = c('Total', 'Visited', 'Remaining', 'Miles Driven', 'Miles Ferried', 
             'Miles Boated', 'Miles Hiked', 'Miles Paddleboarded'),
    Measure = c(nrow(points_sf),
                nrow(visited_sf),
                nrow(data_df) - nrow(visited_sf),
                sum(as.numeric(data_df$Driven), na.rm = TRUE),
                sum(as.numeric(data_df$Ferried), na.rm = TRUE),
                sum(as.numeric(data_df$Boated), na.rm = TRUE),
                sum(as.numeric(data_df$Hiked), na.rm = TRUE),
                sum(as.numeric(data_df$Paddleboarded), na.rm = TRUE))) %>%
    dplyr::mutate(Measure = round(Measure, 0))
  
  saveRDS(summ_df, file.path(getwd(), 'data', 'created', paste0('summary.RDS')))
  
#***************************************************************************************************
#***************************************************************************************************
  