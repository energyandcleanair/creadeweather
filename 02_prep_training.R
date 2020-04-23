require(dplyr)
require(purrr)
require(sf)
require(raster)
require(tibble)
require(lubridate)
source('99_plot.R')
source('99_utils.R')


prep_training <- function(meas_weather, pollutants, deg){
  
  # meas_weather <- readRDS(file.path('data','01_weather','output','meas_w_weather_no2_02.RDS'))
  # saveRDS(meas_weather, file.path('data','01_weather','output','meas_w_weather_025.RDS'))
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
    # We only coalesce weather data where other weather data exists
    weather_rows_idxs <- which(apply(tbl %>% dplyr::select(wd, precip, ceil_hgt, atmos_pres, wd, visibility, air_temp), 1, function(x) any(!is.na(x))))
    if(length(weather_rows_idxs)>0){
      # If all NAs
      if(all(is.na(tbl$ws))) tbl[weather_rows_idxs,]$ws <-0
      if(all(is.na(tbl$precip))) tbl[weather_rows_idxs,]$precip <-0
      if(all(is.na(tbl$ceil_hgt))) tbl[weather_rows_idxs,]$ceil_hgt <-0
      if(all(is.na(tbl$atmos_pres))) tbl[weather_rows_idxs,]$atmos_pres <-0
      if(all(is.na(tbl$wd))) tbl[weather_rows_idxs,]$wd <-0
      if(all(is.na(tbl$visibility))) tbl[weather_rows_idxs,]$visibility <-0
      if(all(is.na(tbl$sunshine))) tbl[weather_rows_idxs,]$sunshine <-0
      if(all(is.na(tbl$air_temp_min))) tbl[weather_rows_idxs,]$air_temp_min <-0
      if(all(is.na(tbl$air_temp_max))) tbl[weather_rows_idxs,]$air_temp_max <-0
      if(all(is.na(tbl$air_temp))) tbl[weather_rows_idxs,]$air_temp <-0
      
      # If only some of them are NAs
      try(tbl <- tbl %>% mutate(sunshine=zoo::na.approx(sunshine, date, na.rm=FALSE)) %>% ungroup(), TRUE)
      try(tbl <- tbl %>% mutate(atmos_pres=zoo::na.approx(atmos_pres, date, na.rm=FALSE)) %>% ungroup(), TRUE)
      try(tbl <- tbl %>% mutate(air_temp_min=zoo::na.approx(air_temp_min, date, na.rm=FALSE)) %>% ungroup(), TRUE)
      try(tbl <- tbl %>% mutate(air_temp_max=zoo::na.approx(air_temp_max, date, na.rm=FALSE)) %>% ungroup(), TRUE)
      try(tbl <- tbl %>% mutate(air_temp=zoo::na.approx(air_temp, date, na.rm=FALSE)) %>% ungroup(), TRUE)
      
      # tbl[weather_rows_idxs,]$precip <- coalesce(tbl[weather_rows_idxs,]$precip, 0) # Precip is a bit special
    }
    
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
  
  filename <- paste('meas_w_weather',paste(tolower(pollutants),collapse='_'),sub('\\.','',deg),'.RDS', sep='_')
  saveRDS(meas_weather, file.path(output_folder, filename))
  
  # Plot number of measurements with weather
  # plot.map_count(meas_weather,
  #                folder=file.path('data', '02_prep_training', 'output'),
  #                title='Number of measurements with weather',
  #                meas_col='meas_weather')
  
  return(meas_weather)
  
  # Quality checks
  # ggplot(meas_weather$meas_weather[[1]] %>% tidyr::gather("key", "value", -c(date, wd_factor)), aes(x=date, y=value)) + 
  #   geom_point(size=0.1) + 
  #   facet_wrap(~key, scales='free_y')
  
    
}

