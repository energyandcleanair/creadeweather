require(dplyr)
require(purrr)
require(sf)
require(raster)
require(tibble)
require(lubridate)
source('99_plot.R')
source('99_utils.R')

meas_weather <- readRDS(file.path('data','01_weather','output','meas_w_weather.RDS'))

# meas <- readRDS(file.path('data','00_init','output','meas_at_gadm.RDS'))
# weather <- readRDS(file.path('data','01_weather','output','gadm1_weather_noaa.RDS'))


cache_folder <- file.path('data', '02_prep_training', 'cache')
if(!dir.exists(cache_folder)) dir.create(cache_folder, recursive = T)

output_folder <- file.path('data', '02_prep_training', 'output')
if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)

input_folder <- file.path('data', '02_prep_training', 'input')
if(!dir.exists(input_folder)) dir.create(input_folder, recursive = T)

# Certain stations miss certain weather variables
coalesce_weather_tbl <- function(tbl){
  # Different kind of treatments
  
  # If all NAs
  if(all(is.na(tbl$wd))) tbl$wd <-0
  if(all(is.na(tbl$precip))) tbl$precip <-0
  if(all(is.na(tbl$ceil_hgt))) tbl$ceil_hgt <-0
  if(all(is.na(tbl$atmos_pres))) tbl$atmos_pres <-0
  if(all(is.na(tbl$wd))) tbl$wd <-0
  if(all(is.na(tbl$visibility))) tbl$visibility <-0
  
  # If only some of them are NAs
  tbl <- tbl %>% mutate(sunshine=zoo::na.approx(sunshine, date, na.rm=FALSE)) %>% ungroup()
  tbl <- tbl %>% mutate(atmos_pres=zoo::na.approx(atmos_pres, date, na.rm=FALSE)) %>% ungroup()
  tbl <- tbl %>% mutate(air_temp_min=zoo::na.approx(air_temp_min, date, na.rm=FALSE)) %>% ungroup()
  tbl <- tbl %>% mutate(air_temp_max=zoo::na.approx(air_temp_max, date, na.rm=FALSE)) %>% ungroup()
  tbl <- tbl %>% mutate(air_temp=zoo::na.approx(air_temp, date, na.rm=FALSE)) %>% ungroup()

  tbl$precip <- coalesce(tbl$precip, 0) # Precip is a bit special
  return(tbl)
}

# Add wind direction factor
enrich_weather_tbl <- function(tbl){
  tbl$wd_factor <- factor(tbl$wd %/% 45)
  return(tbl)
}
# 
meas_weather <- meas_weather %>% rowwise() %>%
 mutate(meas_weather=list(coalesce_weather_tbl(meas_weather))) %>%
 mutate(meas_weather=list(enrich_weather_tbl(meas_weather)))
# 
# # Merge with measurements
# meas_weather <- meas %>%
#   left_join(weather) %>% filter(!is.null(weather[[1]])) %>%
#   rowwise() %>%
#   mutate(meas_weather= list(meas %>% left_join(weather))) %>%
#   dplyr::select(-c(meas,weather))

# Replace NaNs with NA (gbm doesn't like NaNs)
meas_weather <- meas_weather %>% rowwise() %>%
  mutate(meas_weather=list(utils.replace_nan_with_na(meas_weather)))

saveRDS(meas_weather, file.path(output_folder, 'meas_weather.RDS'))

# Plot number of measurements with weather
plot.map_count(meas_weather,
               folder=file.path('data', '02_prep_training', 'output'),
               title='Number of measurements with weather',
               meas_col='meas_weather')
  

