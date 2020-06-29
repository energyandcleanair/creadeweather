

noaa.add_close_stations <- function(meas, n_per_station){

  meas_stations <- meas %>% ungroup() %>%
    dplyr::select(station_id, geometry) %>%
    distinct(station_id, .keep_all=T) %>%
    rowwise() %>%
    mutate(noaa_station=list(
      rnoaa::isd_stations_search(
      lat=sf::st_coordinates(geometry)[,2],
      lon=sf::st_coordinates(geometry)[,1],
      radius=100) %>%
        dplyr::filter(end>=20200101) %>%
        dplyr::arrange(desc(end), distance) %>%
        dplyr::slice(1:n_per_station)
    ))

  meas %>% 
    dplyr::left_join(meas_stations  %>% dplyr::select(-c(geometry)))
}


noaa.get_noaa_at_code <- function(code, years, years_force_refresh=c(2020), cache_folder){
  # Get NOAA data, first trying to use cached files for complete years
  tryCatch({
    # Reading cache files
    years_try_cache <- setdiff(years, years_force_refresh)
    files <- list.files(path=cache_folder, pattern =paste0(code,'_',years_try_cache,'.rds',collapse="|"), full.names = T)
    # files <- file.path(cache_folder, paste0(code,'_',years_try_cache,'.rds'))
    years_cached <- stringr::str_extract(files, "(?<=_)\\d{4}(?=\\.rds)")
    readFile <- function(path){
      if(file.exists(path)) readRDS(path) else NULL
    }
    
    data_cached <- files %>%
      purrr::map_dfr(readFile) %>% 
      bind_rows()
    
    years_to_download <- unique(c(setdiff(years, years_cached), years_force_refresh))
    
    # Downloading fresh data
    if(length(years_to_download)>0){
      data_downloaded <- tryCatch({
        worldmet::importNOAA(
          code = code,
          year = years_to_download,
          hourly = FALSE,
          precip = TRUE,
          PWC = FALSE,
          parallel = F,
          quiet = T,
          path = cache_folder
        )},
        error=function(cond){
        }
      )
    }else{
      data_downloaded <- NULL
    }
    
    
    # Binding and returning
    result <- bind_rows(data_cached, data_downloaded)
    if(nrow(result)==0){return(NULL)}
    
    
    # Aggregate per day
    result <- result %>%
      dplyr::group_by(date=lubridate::date(date)) %>%
      dplyr::summarize(
        air_temp_min=min(air_temp, na.rm=T),
        air_temp_max=max(air_temp, na.rm=T),
        air_temp=mean(air_temp, na.rm=T),
        atmos_pres=mean(atmos_pres, na.rm=T),
        wd=mean(wd, na.rm=T),
        ws_max=max(ws, na.rm=T),
        ws=mean(ws, na.rm=T),
        ceil_hgt=mean(ceil_hgt, na.rm=T),
        visibility=mean(visibility, na.rm=T),
        precip=mean(precip, na.rm=T),
        RH=mean(RH, na.rm=T),
        RH_min=min(RH, na.rm=T),
        RH_max=max(RH, na.rm=T)
      )
      
    return(result)
  }, error=function(err){
    warning(err)
    return(NULL)})
}


noaa.add_weather <- function(meas_w_stations, years=c(2015:2020), years_force_refresh=c(2020), cache_folder){
  
  stations_weather <- meas_w_stations %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols=(noaa_station)) %>%
    dplyr::distinct(station_id, usaf, wban)
  
  stations_weather$code <- paste(stations_weather$usaf, stations_weather$wban, sep="-")
  
  stations_weather$weather <- pbapply::pblapply(stations_weather$code, noaa.get_noaa_at_code,
           years=years, years_force_refresh=years_force_refresh, cache_folder=cache_folder)

  to_date <- function(d){
    tryCatch({
      lubridate::date(d)
    },error=function(err){
      print(d)
      NA
    })
  }
  
  if(nrow(stations_weather %>% rowwise() %>% filter(!is.null(weather)))==0){
    stop("Failed to find weather data")
  }
  meas_w_stations <- meas_w_stations %>%
    left_join(
      stations_weather %>%
        dplyr::select(station_id, weather) %>%
        dplyr::rowwise() %>%
        dplyr::filter("air_temp_min" %in% colnames(weather)) %>%
        ungroup() %>%
        group_by(station_id) %>%
        summarise(weather=list(bind_rows(weather) %>%
                 dplyr::mutate(date=to_date(date)) %>%
                 dplyr::filter(!is.na(date)) %>%
                 dplyr::group_by(date) %>%
                 dplyr::summarize(
                   air_temp_min=min(air_temp_min, na.rm=T),
                   air_temp_max=max(air_temp_max, na.rm=T),
                   air_temp=mean(air_temp, na.rm=T),
                   atmos_pres=mean(atmos_pres, na.rm=T),
                   wd=mean(wd, na.rm=T),
                   ws_max=max(ws_max, na.rm=T),
                   ws=mean(ws, na.rm=T),
                   ceil_hgt=mean(ceil_hgt, na.rm=T),
                   visibility=mean(visibility, na.rm=T),
                   precip=mean(precip, na.rm=T),
                   RH=mean(RH, na.rm=T),
                   RH_min=min(RH, na.rm=T),
                   RH_max=max(RH, na.rm=T)
                 )
            )
        )
    )
    
}


# 
# #-----------------------------------------------------------------------------
# # If no existing result: query the whole dataset again (can take 5-10 hours)
# # If existing results: only query last year and merge into results
# #-----------------------------------------------------------------------------
# gadm1_sf <- sf::st_read('data/00_init/output/gadm1_filtered_bounds.geojson')
# min_year=2015
# max_year=2020
# max_stations_per_gadm1=10
# 
# cache_folder <- file.path('data', '01_weather', 'cache')
# if(!dir.exists(cache_folder)) dir.create(cache_folder)
# 
# output_folder <- file.path('data', '01_weather', 'output')
# if(!dir.exists(output_folder)) dir.create(output_folder)
# 
# 
# 
# create_weather_3h <- function(gadm1_sf, min_year, max_year, max_stations_per_gadm1){
#   
#   gadm1_noaa <- gadm1_sf %>%
#     rowwise() %>%
#     mutate(noaa_station=list(rnoaa::isd_stations_search(bbox = st_bbox(geometry))))
#   
#   gadm1_noaa <- gadm1_noaa %>%
#     mutate(noaa_station_sample=list(noaa_station %>%
#                                       arrange(desc(end), begin) %>%
#                                       slice(1:max_stations_per_gadm1)))
#   
#   gadm1_weather_3h <- gadm1_noaa %>%
#     dplyr::select(gadm1_id, gadm1_name, noaa_station_sample) %>%
#     tidyr::unnest(cols=c(noaa_station_sample)) %>%
#     mutate(weather = purrr::map2(.x=usaf, .y=wban,
#                                  purrr::possibly(~get_noaa(
#                                    code = paste(.x, .y, sep="-"),
#                                    years = min_year:max_year,
#                                    cache_folder=cache_folder
#                                  ),
#                                  otherwise = NA)))
#   # saveRDS(gadm1_weather_3h,file.path('data', '01_weather', 'tmp', 'gadm1_weather_3h.RDS')) #1GB
#   return(gadm1_weather_3h)
# }
# 
# 
# aggregate_weather_3h <- function(gadm1_weather_3h){
# 
#   # Aggregate per day
#   gadm1_weather_day <- gadm1_weather_3h %>% filter(!is.na(weather)) %>%
#     rowwise() %>% mutate(weather=list(
#       weather %>%
#         dplyr::group_by(date=lubridate::date(date)) %>%
#         dplyr::summarize(
#           air_temp=mean(air_temp, na.rm=T),
#           air_temp_min=min(air_temp, na.rm=T),
#           air_temp_max=max(air_temp, na.rm=T),
#           atmos_pres=mean(atmos_pres, na.rm=T),
#           wd=mean(wd, na.rm=T),
#           ws=mean(ws, na.rm=T),
#           ws_max=max(ws, na.rm=T),
#           ceil_hgt=mean(ceil_hgt, na.rm=T),
#           visibility=mean(visibility, na.rm=T),
#           precip=mean(precip, na.rm=T),
#           RH=mean(RH, na.rm=T)
#         )
#       )
#     )
#   
#   # Aggregate per region
#   gadm1_weather_day_region <- gadm1_weather_day %>%
#     dplyr::group_by(gadm1_id, gadm1_name) %>%
#     summarise(weather=list(
#       bind_rows(weather) %>%
#         dplyr::group_by(date) %>%
#         dplyr::summarize(
#           air_temp=mean(air_temp, na.rm=T),
#           air_temp_min=min(air_temp, na.rm=T),
#           air_temp_max=max(air_temp, na.rm=T),
#           atmos_pres=mean(atmos_pres, na.rm=T),
#           wd=mean(wd, na.rm=T),
#           ws=mean(ws, na.rm=T),
#           ws_max=max(ws, na.rm=T),
#           ceil_hgt=mean(ceil_hgt, na.rm=T),
#           visibility=mean(visibility, na.rm=T),
#           precip=mean(precip, na.rm=T),
#           RH=mean(RH, na.rm=T)
#         )
#       )
#     )
#   
#   return(gadm1_weather_day_region)
# }
# 
# add_sunshine <- function(gadm1_weather_day_region){
# 
#   gadm1_sf <- sf::st_read('data/00_init/output/gadm1.geojson')
#   gadm1_centroid_sf <- gadm1_sf %>% rowwise() %>% mutate(geometry=st_centroid(st_as_sfc(st_bbox(geometry))))
# 
#   # Get hourly solar radiation at each day and hour
#   gadm1_sunshine <- gadm1_centroid_sf %>%
#     tidyr::crossing(doy=seq(1:365)) %>%
#     mutate(longitude=purrr::map_dbl(geometry, ~sf::st_coordinates(.x)[[1]])) %>%
#     mutate(latitude=purrr::map_dbl(geometry, ~sf::st_coordinates(.x)[[2]])) %>%
#     mutate(sunshine=purrr::map2_dbl(doy, latitude, ~sirad::extrat(.x, sirad::radians(.y))$ExtraTerrestrialSolarRadiationDaily))
#   
#   gadm1_sunshine_nested <- gadm1_sunshine %>%
#     tidyr::nest(sunshine=c(doy, sunshine)) %>%
#     dplyr::select(gadm1_id, sunshine)
# 
#   # Merge with previous weather data
#   w_sunshine <- gadm1_weather_day_region %>%
#     left_join(gadm1_sunshine_nested) %>%
#     rowwise() %>%
#     mutate(weather= list(
#              weather %>% mutate(doy=lubridate::yday(date)) %>%
#              left_join(sunshine) %>% dplyr::select(-c(doy))
#            )) %>%
#     dplyr::select(-c(sunshine))
#   
#   return(w_sunshine)
# }
# 
# add_pbl <- function(gadm1_weather_day_region){
#   # Adding Planet Boundary Layer height from NCEP
#   # Waitinf for data access
# }
# 
# # Create from scratch: can take 5-10 hours for Europe
# create_weather <- function(gadm1_sf, min_year, max_year, max_stations_per_gadm1){
#   gadm1_weather_3h <- create_weather_3h(gadm1_sf, min_year, max_year, max_stations_per_gadm1)
#   gadm1_weather_day_region <- aggregate_weather_3h(gadm1_weather_3h)
#   gadm1_weather_day_region <- add_sunshine(gadm1_weather_day_region)
#   return(gadm1_weather_day_region)
# }
# 
# # Update previously fetched weather data wih latest measurements (i.e. last year)
# # Not tested yet
# update_weather <- function(gadm1_weather_day_region, gadm1_sf, max_stations_per_gadm1){
#   
#   last_year <- max((gadm1_weather_day_region %>%
#                       rowwise() %>%
#                       mutate(last_year=lubridate::year(max(weather$date))))$last_year)
#   gadm1_weather_3h_new <- create_weather_3h(gadm1_sf, last_year, last_year, max_stations_per_gadm1)
#   gadm1_weather_day_region_new <- aggregate_weather_3h(gadm1_weather_3h_new)
#   gadm1_weather_day_region <- add_sunshine(gadm1_weather_day_region)
#   
#   # Replace in existing results
#   # 1. remove last year in old data
#   gadm1_weather_day_region <-  gadm1_weather_day_region %>%
#     rowwise() %>%
#     mutate(weather=list(weather %>% filter(lubridate::year(date) != last_year)))
#   # 2. add newly fetched data
#   gadm1_weather_day_region <-  bind_rows(gadm1_weather_day_region, gadm1_weather_day_region_new) %>%
#     group_by(gadm1_id, gadm1_name) %>%
#     mutate(weather=list(bind_rows(weather)))
#   
#   return(gadm1_weather_day_region)
# }
# # 
# result_path <- file.path('data','01_weather', 'output','gadm1_weather_noaa.RDS')
# # if(file.exists(result_path)){
# #   gadm1_weather_day_region <- readRDS(result_path)
# #   gadm1_weather_day_region <- update_weather(gadm1_weather_day_region, gadm1_sf, max_stations_per_gadm1)
# # }else{
#   gadm1_weather_day_region <- create_weather(gadm1_sf, min_year, max_year, max_stations_per_gadm1)
# # }
# 
# saveRDS(gadm1_weather_day_region, result_path)
