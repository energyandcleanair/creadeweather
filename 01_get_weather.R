require(purrr)
require(sf)
require(dplyr)

source('01_get_weather_noaa.R')
source('01_get_weather_ncar.R')

if(!require(install.load)) install.packages("install.load"); require(install.load)
install_load('rnoaa', 'worldmet', 'sirad')


cache_folder <- file.path('data', '01_weather', 'cache')
if(!dir.exists(cache_folder)) dir.create(cache_folder, recursive = T)

output_folder <- file.path('data', '01_weather', 'output')
if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)

input_folder <- file.path('data', '01_weather', 'input')
if(!dir.exists(input_folder)) dir.create(input_folder, recursive = T)


meas <- readRDS(file.path('data', '00_init', 'output', 'eea_meas_daily.RDS'))


# For development purposes
meas <- meas %>% head(5)

meas_light <- meas %>% ungroup() %>% dplyr::select(station_id, geometry) %>% distinct(station_id, .keep_all = T)
meas_w_stations <- noaa.add_close_stations(meas_light, n_per_station = 1)
meas_w_weather <- noaa.add_weather(meas_w_stations, years=c(2015:2020),
                                   years_force_refresh = NULL,#c(2020),
                                   cache_folder = cache_folder)

# Add Planet Boundary Layer from ncar
meas_w_weather <- ncar.add_pbl(meas_w_weather)

saveRDS(meas_w_weather, file.path(output_folder, 'meas_w_weather.RDS'))

