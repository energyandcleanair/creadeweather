require(purrr)
require(sf)
require(dplyr)

if(!require(install.load)) install.packages("install.load"); require(install.load)
install_load('rnoaa', 'worldmet', 'sirad')


#-----------------------------------------------------------------------------
# If no existing result: query the whole dataset again (can take 5-10 hours)
# If existing results: only query last year and merge into results
#-----------------------------------------------------------------------------
gadm1_sf <- sf::st_read('data/00_init/output/gadm1_filtered_bounds.geojson')
min_year=2015
max_year=2020
max_stations_per_gadm1=10

cache_folder <- file.path('data', '01_weather', 'cache')
if(!dir.exists(cache_folder)) dir.create(cache_folder)

output_folder <- file.path('data', '01_weather', 'output')
if(!dir.exists(output_folder)) dir.create(output_folder)

get_noaa <- function(code, years, cache_folder){
  # Get NOAA data, first trying to use cached files for complete years
  year_incomplete <- c(2020)
  
  # Reading cache files
  years_try_cache <- setdiff(years, year_incomplete)
  files <- file.path(cache_folder, paste0(code,'_',years_try_cache,'.rds'))
  
  readFile <- function(path){
    if(file.exists(path)) readRDS(path) else NULL
  }

  data_cached <- files %>% map_dfr(readFile) %>% 
    bind_rows()
  
  # Adding last year
  data_last_year <- worldmet::importNOAA(
    code = code,
    year = year_incomplete,
    hourly = FALSE,
    precip = TRUE,
    PWC = FALSE,
    parallel = T,
    quiet = FALSE,
    path = cache_folder
  )
  
  # Binding and returning
  return(bind_rows(data_cached, data_last_year))
}

create_weather_3h <- function(gadm1_sf, min_year, max_year, max_stations_per_gadm1){
  
  gadm1_noaa <- gadm1_sf %>%
    rowwise() %>%
    mutate(noaa_station=list(rnoaa::isd_stations_search(bbox = st_bbox(geometry))))
  
  gadm1_noaa <- gadm1_noaa %>%
    mutate(noaa_station_sample=list(noaa_station %>%
                                      arrange(desc(end), begin) %>%
                                      slice(1:max_stations_per_gadm1)))
  
  gadm1_weather_3h <- gadm1_noaa %>%
    dplyr::select(gadm1_id, gadm1_name, noaa_station_sample) %>%
    tidyr::unnest(cols=c(noaa_station_sample)) %>%
    mutate(weather = purrr::map2(.x=usaf, .y=wban,
                                 purrr::possibly(~get_noaa(
                                   code = paste(.x, .y, sep="-"),
                                   years = min_year:max_year,
                                   cache_folder=cache_folder
                                 ),
                                 otherwise = NA)))
  # saveRDS(gadm1_weather_3h,file.path('data', '01_weather', 'tmp', 'gadm1_weather_3h.RDS')) #1GB
  return(gadm1_weather_3h)
}


aggregate_weather_3h <- function(gadm1_weather_3h){

  # Aggregate per day
  gadm1_weather_day <- gadm1_weather_3h %>% filter(!is.na(weather)) %>%
    rowwise() %>% mutate(weather=list(
      weather %>%
        dplyr::group_by(date=lubridate::date(date)) %>%
        dplyr::summarize(
          air_temp=mean(air_temp, na.rm=T),
          air_temp_min=min(air_temp, na.rm=T),
          air_temp_max=max(air_temp, na.rm=T),
          atmos_pres=mean(atmos_pres, na.rm=T),
          wd=mean(wd, na.rm=T),
          ws=mean(ws, na.rm=T),
          ws_max=max(ws, na.rm=T),
          ceil_hgt=mean(ceil_hgt, na.rm=T),
          visibility=mean(visibility, na.rm=T),
          precip=mean(precip, na.rm=T),
          RH=mean(RH, na.rm=T)
        )
      )
    )
  
  # Aggregate per region
  gadm1_weather_day_region <- gadm1_weather_day %>%
    dplyr::group_by(gadm1_id, gadm1_name) %>%
    summarise(weather=list(
      bind_rows(weather) %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(
          air_temp=mean(air_temp, na.rm=T),
          air_temp_min=min(air_temp, na.rm=T),
          air_temp_max=max(air_temp, na.rm=T),
          atmos_pres=mean(atmos_pres, na.rm=T),
          wd=mean(wd, na.rm=T),
          ws=mean(ws, na.rm=T),
          ws_max=max(ws, na.rm=T),
          ceil_hgt=mean(ceil_hgt, na.rm=T),
          visibility=mean(visibility, na.rm=T),
          precip=mean(precip, na.rm=T),
          RH=mean(RH, na.rm=T)
        )
      )
    )
  
  return(gadm1_weather_day_region)
}

add_sunshine <- function(gadm1_weather_day_region){

  gadm1_sf <- sf::st_read('data/00_init/output/gadm1.geojson')
  gadm1_centroid_sf <- gadm1_sf %>% rowwise() %>% mutate(geometry=st_centroid(st_as_sfc(st_bbox(geometry))))

  # Get hourly solar radiation at each day and hour
  gadm1_sunshine <- gadm1_centroid_sf %>%
    tidyr::crossing(doy=seq(1:365)) %>%
    mutate(longitude=purrr::map_dbl(geometry, ~sf::st_coordinates(.x)[[1]])) %>%
    mutate(latitude=purrr::map_dbl(geometry, ~sf::st_coordinates(.x)[[2]])) %>%
    mutate(sunshine=purrr::map2_dbl(doy, latitude, ~sirad::extrat(.x, sirad::radians(.y))$ExtraTerrestrialSolarRadiationDaily))
  
  gadm1_sunshine_nested <- gadm1_sunshine %>%
    tidyr::nest(sunshine=c(doy, sunshine)) %>%
    dplyr::select(gadm1_id, sunshine)

  # Merge with previous weather data
  w_sunshine <- gadm1_weather_day_region %>%
    left_join(gadm1_sunshine_nested) %>%
    rowwise() %>%
    mutate(weather= list(
             weather %>% mutate(doy=lubridate::yday(date)) %>%
             left_join(sunshine) %>% dplyr::select(-c(doy))
           )) %>%
    dplyr::select(-c(sunshine))
  
  return(w_sunshine)
}

add_pbl <- function(gadm1_weather_day_region){
  # Adding Planet Boundary Layer height from NCEP
  # Waitinf for data access
}

# Create from scratch: can take 5-10 hours for Europe
create_weather <- function(gadm1_sf, min_year, max_year, max_stations_per_gadm1){
  gadm1_weather_3h <- create_weather_3h(gadm1_sf, min_year, max_year, max_stations_per_gadm1)
  gadm1_weather_day_region <- aggregate_weather_3h(gadm1_weather_3h)
  gadm1_weather_day_region <- add_sunshine(gadm1_weather_day_region)
  return(gadm1_weather_day_region)
}

# Update previously fetched weather data wih latest measurements (i.e. last year)
# Not tested yet
update_weather <- function(gadm1_weather_day_region, gadm1_sf, max_stations_per_gadm1){
  
  last_year <- max((gadm1_weather_day_region %>%
                      rowwise() %>%
                      mutate(last_year=lubridate::year(max(weather$date))))$last_year)
  gadm1_weather_3h_new <- create_weather_3h(gadm1_sf, last_year, last_year, max_stations_per_gadm1)
  gadm1_weather_day_region_new <- aggregate_weather_3h(gadm1_weather_3h_new)
  gadm1_weather_day_region <- add_sunshine(gadm1_weather_day_region)
  
  # Replace in existing results
  # 1. remove last year in old data
  gadm1_weather_day_region <-  gadm1_weather_day_region %>%
    rowwise() %>%
    mutate(weather=list(weather %>% filter(lubridate::year(date) != last_year)))
  # 2. add newly fetched data
  gadm1_weather_day_region <-  bind_rows(gadm1_weather_day_region, gadm1_weather_day_region_new) %>%
    group_by(gadm1_id, gadm1_name) %>%
    mutate(weather=list(bind_rows(weather)))
  
  return(gadm1_weather_day_region)
}

result_path <- file.path('data','01_weather', 'output','gadm1_weather_noaa.RDS')
# if(file.exists(result_path)){
#   gadm1_weather_day_region <- readRDS(result_path)
#   gadm1_weather_day_region <- update_weather(gadm1_weather_day_region, gadm1_sf, max_stations_per_gadm1)
# }else{
  gadm1_weather_day_region <- create_weather(gadm1_sf, min_year, max_year, max_stations_per_gadm1)
# }

saveRDS(gadm1_weather_day_region, result_path)
