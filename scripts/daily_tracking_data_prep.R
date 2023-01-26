library(googlesheets4)
library(magrittr)

gs_link <- 'https://docs.google.com/spreadsheets/d/1-yy4BJaRCBhQa8IYqeOWpRmGUyLccBgSwi4AdeWvZ1k/'

data2021_df <- read.csv(file.path(getwd(), 'data', 'raw', 'tracking_2021.csv'))
data2022_df <- read.csv(file.path(getwd(), 'data', 'raw', 'tracking_2022.csv'))
data2023_df <- googlesheets4::range_read(gs_link, sheet = "Daily")
data2023_wdf <- googlesheets4::range_read(gs_link, sheet = "Weekly")

mets_ratios <- data.frame(
  activity = c('run', 'bike', 'walk', 'OAC', 'rowing', 'stairs', 'paddleboard', 'nordictrac'),
  mets = c(12.5, 10, 4, 5, 7, 7, 5.5, 7),
  conversion = c(7.5, 15, 4, .5, 16000, 700, 3, 60)
)

### Clean up

fixNA <- function(x) ifelse(is.na(x), 0, x)

## 2021

  data2021_ddf <-
    data2021_df %>%
    dplyr::mutate(Date = as.Date(Date, format = '%m/%d/%y'),
                  Stretch = ifelse(Stretch == 'Yes', 1, 0),
                  dplyr::across(Sleep:Drink, fixNA),
                  Strength = ifelse(Abs >= 100 | Pushups >= 30 |
                                      Curls >= 30, 1, 0),
                  Location = ifelse(Location == '', 'Home', Location))
  asd(data2021_ddf, 6)
  summary(data2021_ddf)
  
## 2022
  
  data2022_ddf <-
    data2022_df %>%
    dplyr::mutate(Date = as.Date(Date, format = '%m/%d/%y'),
                  dplyr::across(Sleep:Fasting.Day, fixNA),
                  Strength = ifelse(Abs >= 100 | Pushups >= 30 |
                                      Curls >= 30, 1, 0),
                  Strength = ifelse(OAC, 1, Strength),
                  Location = ifelse(Location == '', 'Home', Location)) %>%
    dplyr::rename(Stairs = Flights.of.Stairs,
                  NordicTrac = Min.on.Nordic.Trac,
                  Fasting = Fasting.Day)
  asd(data2022_ddf, 6)
  summary(data2022_ddf)
  
### 2023

  data2023_ddf <-
    data2023_df %>%
    dplyr::select(-c(Day)) %>%
    dplyr::mutate(Date = as.Date(Date),
                  dplyr::across(Sleep:OAC, fixNA),
                  Strength = ifelse(Abs >= 100 | Pushups >= 30 |
                                      Curls >= 30, 1, 0),
                  Strength = ifelse(OAC, 1, Strength)) %>%
    dplyr::filter(Date <= Sys.Date())
  asd(data2023_ddf, 6)
  summary(data2023_ddf)

### Combine
  
data2122_ddf <- 
  data2021_ddf %>%
  dplyr::bind_rows(data2022_ddf) %>%
  dplyr::select(Date, Location, Sleep, Food, Fasting, Drink, xmx, Weight, Medit, Create, Read, Pickups, 
                Run, Walk, Bike, Stretch, Strength, Pushups, Curls, Squats, Abs, OAC, Stairs, 
                Rowing, Paddleboard, NordicTrac) %>%
  dplyr::mutate(Year = lubridate::year(Date),
                Relative_Year = Year - min(Year),
                Week = lubridate::week(Date),
                Week = ifelse(Week == 53, 52, Week),
                Total_Weeks = Week + (Relative_Year * 52))
asd(data2122_ddf)

data2122_ddf %>% dplyr::group_by(Total_Weeks) %>%
  dplyr::summarize(Weight = mean(Weight, na.rm = TRUE),
                   Sleep = mean(Sleep, na.rm = TRUE),
                   Run = mean(Run, na.rm = TRUE)) -> x



  
  
  