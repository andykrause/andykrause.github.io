
library(tidyverse)
library(tidycensus)
library(sf)
library(OpenStreetMap)
library(ggmap)
library(raster)
library(rgdal)




  county_visit = c('king', 'snohomish', 'kittitas', 'island', 'san juan', 'kitsap', 'pierce',
                   'skagit', 'thurston')

  if (F){
     var <- 'B02001_001'
     census_api_key("eba9fd9d7bd7f7164ed95b4c7f8874cf37586d42")
      wa_sf <- tidycensus::get_acs(geography = 'county',
                                   state = 'wa',
                                   variables = var,
                               year = 2018,
                               geometry = T)
      
      wa_sf <- 
        wa_sf %>%
        mutate(county = str_remove(str_remove(NAME, ' County.*'),
                                   'Census Tract.*, ')) %>%
        mutate(state = str_remove(NAME, '.*County, ')) %>%
        select(-NAME) %>%
        dplyr::select(GEOID,county, geometry)
      
      saveRDS(wa_sf, file.path(getwd(), 'data', 'county_boundaries.RDS'))
      
      parks_poly_sf <- sf::st_read(file.path(getwd(), 'data', 'State_parks.shp')) %>%
        sf::st_transform(., crs = 4269)
      sf::st_crs(parks_poly_sf) <- 4269
      
      saveRDS(parks_poly_sf, file.path(getwd(), 'data', 'park_boundaries.RDS'))
      
  } else {
    
    wa_sf <- readRDS(file.path(getwd(), 'data', 'county_boundaries.RDS'))
    parks_df <- readr::read_csv(file.path(getwd(), 'data', 'final_parks_data.csv')) %>%
      dplyr::filter(Challenge == 1) %>%
      dplyr::mutate(Visited = as.factor(Visited))
    parks_sf <- sf::st_as_sf(parks_df, coords = c('Longitude', 'Latitude'))
    st_crs(parks_sf) <- 4269
    parks_poly_sf <- readRDS(file.path(getwd(), 'data', 'park_boundaries.RDS'))
  }
  
  
  wa_sf <- wa_sf %>%
    mutate(visited = as.factor(ifelse(tolower(county) %in% county_visit, 1, 0)))
  
 state_map <-   
 ggplot(data = wa_sf) + 
   geom_sf(aes(fill = visited), fill = 'gray90', color = 'white') + 
   scale_fill_manual(name = 'Visited', values = c('white', 'gray70')) + 
   theme(axis.title=element_blank(),
         axis.text=element_blank(),
         axis.ticks=element_blank(),
         panel.border = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         legend.position = 'none') + 
   geom_sf(data = parks_sf, aes(color = Visited, size = Visited, shape = Visited)) + 
   scale_color_manual(values = c('gray10', 'green4')) + 
   scale_size_manual(values = c(1.1, 2.6)) + 
   scale_shape_manual(values = c(20, 17))
 
 
 png(filename = file.path(getwd(), 'images', 'state_map.png'), width = 780, height = 480)
   state_map
 dev.off()
 
#################################################################################################### 
### Individual Park Maps ---------------------------------------------------------------------------
 
 ## Parameters
 image_width <- 800
 image_height <- 800
 cp_scale <- 500 # Width of boundary to create circle
 c_scale <- 1.6  # Scale of lat to long to create square
 x_scale <- .15 # Map zoom scale (width in lon)
 
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
 
  ## Visited
  visited_sf <- parks_sf %>%
     dplyr::filter(Visited == 1)
  
  ## Print Maps
  for (k in 1:nrow(visited_sf)){
     i_sf <- visited_sf[k,]
     cat('Drawing: ', i_sf$ParkName, '\n')
     shortname <- tolower(i_sf$ParkName)
     shortname <- gsub(' ', '', shortname)
     png(filename = file.path(getwd(), 'images', paste0('map_', shortname, '.png')), 
                              width = 800, height = 800)
       plotPark(i_sf$ParkCode,
                scale=.15, map_type = 'toner', park_color = 'red', 
                circle = TRUE, cscale = 1.6, cpscale=500)
     dev.off()
  }
  
  
### Clark Island ----------------------------------------------------------------------------
  
 png(filename = file.path(getwd(), 'images', 'map_clarkisland.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Clark Island', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500)
 dev.off()
 
 # Posey
 png(filename = file.path(getwd(), 'images', 'map_poseyisland.png'), width = 800, height = 800)
 plotPark(parks_sf$ParkCode[grep('Posey', parks_sf$ParkName)],
          scale=.15, map_type = 'toner', park_color = 'red', 
          circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 
 # Stuart
 png(filename = file.path(getwd(), 'images', 'map_stuartisland.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Stuart', parks_sf$ParkName)],
            scale=.2, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 
 # Moran
 png(filename = file.path(getwd(), 'images', 'map_moran.png'), width = 800, height = 800)
 plotPark(parks_sf$ParkCode[grep('Moran', parks_sf$ParkName)],
          scale=.15, map_type = 'toner', park_color = 'red', 
          circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 # Sucia
 png(filename = file.path(getwd(), 'images', 'map_suciaisland.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Sucia', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 
 # Obstruction
 png(filename = file.path(getwd(), 'images', 'map_obstructionpass.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Obstruct', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 # Blake Island
 png(filename = file.path(getwd(), 'images', 'map_blakeisland.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Blake', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 # Blind Island
 png(filename = file.path(getwd(), 'images', 'map_blindisland.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Blind', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 # Jones Island
 png(filename = file.path(getwd(), 'images', 'map_jonesisland.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Jones', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 # Turn Island
 png(filename = file.path(getwd(), 'images', 'map_turnisland.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Turn', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 # Tolmie Island
 png(filename = file.path(getwd(), 'images', 'map_tolmie.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Tolmie', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 # Millersylvania
 png(filename = file.path(getwd(), 'images', 'map_millersylvania.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Miller', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 # Lake Easton
 png(filename = file.path(getwd(), 'images', 'map_lakeeaston.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Easton', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 # Gingko
 png(filename = file.path(getwd(), 'images', 'map_ginkgo.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Ginkgo', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 # Olmstead
 png(filename = file.path(getwd(), 'images', 'map_olmsteadplace.png'), width = 800, height = 800)
   plotPark(parks_sf$ParkCode[grep('Olmstead', parks_sf$ParkName)],
            scale=.15, map_type = 'toner', park_color = 'red', 
            circle = TRUE, cscale = 1.6, cpscale=500, spin = 12)
 dev.off()
 
 
 # Palouse
 # Olallie
 # Joseph Whidbey
 # Fort Ebey
 # Fort Casey
 # South Whidbey
 # Federation
 # Nolte
 # Kanaskat
 # Flaming Geyser
 # Squak
 png(filename = file.path(getwd(), 'images', 'map_squakmountain.png'), width = 780, height = 480)
   plotPark(parks_sf$ParkCode[grep('Squak', parks_sf$ParkName)],
            scale=.22, map_type = 'toner', park_color = 'red')
 dev.off()
 
 # Camano Island
 png(filename = file.path(getwd(), 'images', 'map_camanoisland.png'), width = 780, height = 480)
   plotPark(parks_sf$ParkCode[grep('Camano', parks_sf$ParkName)],
            scale=.22, map_type = 'toner', park_color = 'red')
 dev.off()
 
 # Cama Beach
 png(filename = file.path(getwd(), 'images', 'map_camabeach.png'), width = 780, height = 480)
   plotPark(parks_sf$ParkCode[grep('Cama B', parks_sf$ParkName)],
            scale=.3, map_type = 'toner', park_color = 'red')
 dev.off()
 
 # Deception Pass
  png(filename = file.path(getwd(), 'images', 'map_deceptionpass.png'), width = 780, height = 480)
    plotPark(parks_sf$ParkCode[grep('Deception', parks_sf$ParkName)],
             scale=.41, map_type = 'toner', park_color = 'red')
  dev.off()
 
 # Dash Point
 png(filename = file.path(getwd(), 'images', 'map_dashpoint.png'), width = 780, height = 480)
   plotPark(parks_sf$ParkCode[grep('Dash', parks_sf$ParkName)],
            scale=.2, map_type = 'toner', park_color = 'red')
 dev.off()
 
 # Saltwater
 png(filename = file.path(getwd(), 'images', 'map_saltwater.png'), width = 780, height = 480)
   plotPark(parks_sf$ParkCode[grep('Saltwater', parks_sf$ParkName)],
            scale=.2, map_type = 'toner', park_color = 'red')
 dev.off()
 
 # Lake Samm
 png(filename = file.path(getwd(), 'images', 'map_lakesammamish.png'), width = 780, height = 480)
  plotPark(parks_sf$ParkCode[grep('Samm', parks_sf$ParkName)],
           scale=.23, map_type = 'toner', park_color = 'red')
 dev.off()
 
 # Saint Edward
 png(filename = file.path(getwd(), 'images', 'map_saintedward.png'), width = 780, height = 480)
   plotPark(parks_sf$ParkCode[grep('Edward', parks_sf$ParkName)],
            scale=.23, map_type = 'toner', park_color = 'red')
 dev.off()
 
 # Spencer Spit
 png(filename = file.path(getwd(), 'images', 'map_spencerspit.png'), width = 780, height = 480)
   plotPark(parks_sf$ParkCode[grep('Spencer', parks_sf$ParkName)],
            scale=.23, map_type = 'toner', park_color = 'red')
 dev.off()
 

 
 # courtesy R Lovelace
 ggmap_rast <- function(map){
   map_bbox <- attr(map, 'bb') 
   .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
   my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
   rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
   red <- my_map
   values(red) <- rgb_cols[['red']]
   green <- my_map
   values(green) <- rgb_cols[['green']]
   blue <- my_map
   values(blue) <- rgb_cols[['blue']]
   stack(red,green,blue)
 }
 
 rast_map <- ggmap_rast(map = basemap) # convert google map to raster object
 
 makeCircle <- function(center_sf, 
                        radius, 
                        nPoints = 1000,
                        spin = 0){
   # centers: the data frame of centers with ID
   # radius: radius measured in kilometer
   #
   meanLat <- mean(center_sf$Latitude)
   # length per longitude changes with lattitude, so need correction
   radiusLon <- radius / cos(meanLat/57.3) 
   radiusLat <- radius
   circleDF <- data.frame(ID = rep(1, each = nPoints))
   angle <- seq(0 + spin, (2*pi) + spin, length.out = nPoints)
   
   circleDF$lon <- center_sf$Longitude + radiusLon * cos(angle)
   circleDF$lat <- center_sf$Latitude + radiusLat * sin(angle)
   return(circleDF)
 }
 cc <- makeCircle(park_df, .1) %>%
   st_as_sf(., coords = c('lon', 'lat')) 
 
 cc2 <- makeCircle(park_df, .1, spin = 2) %>%
   st_as_sf(., coords = c('lon', 'lat')) 
 
 polys = cc %>% 
   dplyr::group_by(ID) %>% 
   dplyr::summarise() %>%
   st_cast("POLYGON")
 
 dc.only <- raster::mask(rast_map, polys) # clip to bounds of census tracts
 dc.df <- data.frame(rasterToPoints(dc.only))
 ggplot(dc.df) + 
   geom_point(aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255))) + 
   scale_color_identity()
 
 
 library(googleVis)
 library(R2HTML)
 url <- c('http://nytimes.com', 'http://cnn.com', 'http://www.weather.gov')
 urlTitles <- c('NY Times', 'CNN', 'Weather')
 foo <- data.frame(a=c(1,2,3), b=c(4,5,6), url=url)
 foo <- transform(foo, url = paste('<a href = ', shQuote(url), '>', urlTitles, '</a>')) 
 x   <- gvisTable(foo, options = list(allowHTML = TRUE))
 plot(x)
 
 
 library(DT)
 library(tidyverse)
 
 tbl <- 
    tibble::tibble(
       ~url,
       'Dead',
       '<a href="https://ups.com">UPS</a>')
 
 tbl %>% DT::datatable(escape = FALSE, rownames = FALSE)
 
 
 test_data <- tribble(
    ~id, ~link,
    "Example link #1", "https://google.ca",
    "Example link #2", "https://community.rstudio.com"
 ) %>%
    dplyr::mutate(
       link = glue(
          '=HYPERLINK("{link}", "{id}")'
       )
    )
 
 
 