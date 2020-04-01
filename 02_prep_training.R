require(dplyr)
require(purrr)
require(sf)
require(raster)
require(tibble)
require(lubridate)
source('99_plot.R')

meas <- readRDS(file.path('data','00_init','output','meas_at_gadm.RDS'))
weather <- readRDS(file.path('data','01_weather','output','gadm1_weather_noaa.RDS'))



# Certain stations miss certain weather variables
coalesce_weather_tbl <- function(tbl){
  if(all(is.na(tbl$wd))) tbl$wd <-0
  if(all(is.na(tbl$precip))) tbl$precip <-0
  if(all(is.na(tbl$ceil_hgt))) tbl$ceil_hgt <-0
  if(all(is.na(tbl$atmos_pres))) tbl$atmos_pres <-0
  if(all(is.na(tbl$wd))) tbl$wd <-0
  if(all(is.na(tbl$visibility))) tbl$visibility <-0
  # tbl <- tbl %>% mutate(atmos_pres=zoo::na.approx(atmos_pres, date, na.rm=FALSE)) %>% ungroup()
  # tbl$atmos_pres <- coalesce(tbl$atmos_pres, 1030) # If still NA everywhere
  return(tbl)
}

# Add wind direction factor
enrich_weather_tbl <- function(tbl){
  tbl$wd_factor <- factor(tbl$wd %/% 45)
  return(tbl)
}
weather <- weather %>% rowwise() %>%
  mutate(weather=list(coalesce_weather_tbl(weather))) %>%
  mutate(weather=list(enrich_weather_tbl(weather)))

# Merge with measurements
meas_weather <- meas %>%
  left_join(weather) %>% filter(!is.null(weather[[1]])) %>%
  rowwise() %>%
  mutate(meas_weather= list(meas %>% left_join(weather))) %>%
  dplyr::select(-c(meas,weather))

# Replace NaNs with NA (gbm doesn't like NaNs)
meas_weather <- meas_weather %>% rowwise() %>%
  mutate(meas_weather=list(utils.replace_nan_with_na(meas_weather)))

saveRDS(meas_weather, file.path('data', '02_prep_training', 'output', 'meas_weather_gadm1.RDS'))

# Plot number of measurements with weather
plot.map_count(meas_weather,
               folder=file.path('data', '02_prep_training', 'output'),
               title='Number of measurements with weather',
               meas_col='meas_weather')
  

