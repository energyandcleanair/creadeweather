require(dplyr)
require(purrr)
require(sf)
require(raster)
require(tibble)
require(lubridate)

meas <- readRDS(file.path('data','00_init','output','meas_at_gadm.RDS'))
weather <- readRDS(file.path('data','01_weather','output','gadm1_weather_noaa.RDS'))
  
# Split by pollutant
meas <- meas %>% tidyr::unnest(cols=c(meas)) %>%
  rename(value=Concentration) %>%
  group_by(gadm1_id, gadm1_name, AirPollutant) %>%
  tidyr::nest() %>% rename(meas=data)
  

# Merge with measurements
meas_weather <- meas %>%
  left_join(weather) %>% filter(!is.null(weather[[1]])) %>%
  rowwise() %>%
  mutate(meas_weather= list(meas %>% left_join(weather))) %>%
  dplyr::select(-c(meas,weather))

saveRDS(meas_weather, file.path('data', '02_prep_training', 'output', 'meas_weather_gadm1.RDS'))

