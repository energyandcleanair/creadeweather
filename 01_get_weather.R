require(purrr)
require(sf)
require(dplyr)

if(!require(install.load)) install.packages("install.load"); require(install.load)
install_load('rnoaa', 'worldmet', 'sirad')


cache_folder <- file.path('data', '01_weather', 'cache')
if(!dir.exists(cache_folder)) dir.create(cache_folder, recursive = T)

output_folder <- file.path('data', '01_weather', 'output')
if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)

input_folder <- file.path('data', '01_weather', 'input')
if(!dir.exists(input_folder)) dir.create(input_folder, recursive = T)


meas <- readRDS(file.path('data', '00_init', 'output', 'eea_meas_daily.RDS'))
meas_w_stations <- noaa.add_close_stations(meas, n_per_station = 1)
meas_w_weather <- noaa.add_weather(meas_w_stations, years=c(2015:2020),
                                   years_force_refresh = c(2020),
                                   cache_folder = cache_folder)
                                   


