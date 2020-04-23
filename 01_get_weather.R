require(purrr)
require(sf)
require(dplyr)

source('01_get_weather_noaa.R')
source('01_get_weather_ncar.R')
source('01_get_weather_sirad.R')


get_weather <- function(meas, pollutants, deg){
    
  if(!require(install.load)) install.packages("install.load"); require(install.load)
  install_load('rnoaa', 'worldmet', 'sirad')
  
  
  cache_folder <- file.path('data', '01_weather', 'cache')
  if(!dir.exists(cache_folder)) dir.create(cache_folder, recursive = T)
  
  output_folder <- file.path('data', '01_weather', 'output')
  if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
  
  input_folder <- file.path('data', '01_weather', 'input')
  if(!dir.exists(input_folder)) dir.create(input_folder, recursive = T)
  
  # meas <- readRDS(file.path('data', '00_init', 'output', 'eea_meas_daily_no2_02.RDS'))
  
  
  # For development purposes
  # meas <- meas %>% head(5)
  print("Getting NOAA")
  stations <- meas %>% ungroup() %>% dplyr::select(station_id, geometry) %>% distinct(station_id, .keep_all = T)
  stations_w_noaa <- noaa.add_close_stations(stations, n_per_station = 2)
  weather <- noaa.add_weather(stations_w_noaa, years=c(2015:2020),
                                     years_force_refresh = NULL, #2020, #c(2020),
                                     cache_folder = cache_folder)
  
  # Add Planet Boundary Layer from ncar
  print("Getting Plamet Boundary Layer")
  weather <- ncar.add_pbl(weather)
  
  
  # Add sunshine
  print("Getting Sunshine")
  weather <- sirad.add_sunshine(weather)
  
  # Join weather into measurements
  meas_w_weather <- meas %>%
    left_join(weather %>% dplyr::select(station_id, weather)) %>%
    rowwise() %>%
    mutate(meas=list(
      meas %>% left_join(weather) %>% select(-c(iso2, station_id))
    )) %>%
    rename(meas_weather=meas) %>%
    dplyr::select(-c(weather))
 
  
  filename <- paste('meas_w_weather',paste(tolower(pollutants),collapse='_'),sub('\\.','',deg),'.RDS',sep='_')
  saveRDS(meas_w_weather, file.path(output_folder, filename))
  return(meas_w_weather)
}

