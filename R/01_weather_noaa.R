noaa.add_weather <- function(location_dates,
                             weather_vars,
                             n_per_location,
                             years_force_refresh){
  
  # Find weather stations nearby
  locations_w_stations <- noaa.add_close_stations(location_dates=location_dates,
                                                  n_per_location = n_per_location)
  
  # Get weather at these stations
  locations_weather <- noaa.collect_weather(locations_w_stations,
                                            weather_vars=weather_vars,
                                            years_force_refresh = years_force_refresh)
  
  # Ensure columns are preserved
  result <- location_dates %>%
    select(-c(weather)) %>%
    left_join(locations_weather)
  
  return(result)
}


noaa.add_close_stations <- function(location_dates, n_per_location){

  suppressMessages(location_dates %>%
    rowwise() %>%
    mutate(noaa_station=list(
      rnoaa::isd_stations_search(
      lat=sf::st_coordinates(geometry)[,2],
      lon=sf::st_coordinates(geometry)[,1],
      radius=150) %>% #radius is in km
        dplyr::filter(end>=20200101) %>%
        dplyr::arrange(desc(end), distance) %>%
        dplyr::slice(1:n_per_location)
    )))
}

noaa.available_years <- function(code){

  # Update cache file if from previous year (we give NOAA 10 days to update it)
  n_days <- 10
  f <- rnoaa::isd_cache$cache_path_get()
  refresh <- !file.exists(f) | 
    (file.info(f)$ctime < 
       min(lubridate::today(),
           lubridate::floor_date(lubridate::today(), "year") + n_days))
    
  d <- suppressMessages(rnoaa::isd_stations(refresh=refresh)) %>%
    filter(paste0(usaf,"-",wban)==!!code) %>%
    summarise(begin=as.integer(min(begin/10000)),
              end=as.integer(max(end/10000)))
  
  seq(d$begin, d$end)
}

noaa.valid_years_cached <- function(code, years, cache_folder){
  files <- list.files(path=cache_folder, pattern =paste0(code,'_',years,'.rds',collapse="|"), full.names = T)
  if(length(files)==0){
   return(c()) 
  }
    
  years <- as.numeric(stringr::str_extract(files, "(?<=_)\\d{4}(?=\\.rds)"))
  # Only keep files who have been updated after the end of the year
  # But also keep those who have been updated today
  
  is_valid <- file.info(files)$mtime >= pmin(as.POSIXct(paste0(years+1,"-01-01")),
                                            lubridate::today())
  return(years[is_valid])
}


noaa.get_noaa_at_code <- function(code, year_from, year_to, years_force_refresh=NULL, cache_folder){
  
  # Get NOAA data, first trying to use cached files for complete years
  tryCatch({
    # Reading cache files
    years <- seq(year_from, year_to)
    years_available <- noaa.available_years(code)
    years <- intersect(years, years_available)
    years_try_cache <- setdiff(years, years_force_refresh)
    years_cached <- noaa.valid_years_cached(code, years_try_cache, cache_folder)
    files_cached <- list.files(path=cache_folder, pattern =paste0(code,'_',years_cached,'.rds',collapse="|"), full.names = T)
    readFile <- function(path){
      if(file.exists(path)) readRDS(path) else NULL
    }
    
    data_cached <- files_cached %>%
      purrr::map_dfr(readFile) %>% 
      bind_rows()
    
    years_to_download <- unique(c(setdiff(years, years_cached), years_force_refresh))

    download_data <- function(year, code, cache_folder){

      tryCatch({
        worldmet::importNOAA(
          code = code,
          year = year,
          quiet = T,
          path = cache_folder
          )},
        error=function(cond){
          warning(paste("Failed to get new data for years", years_to_download,". Using cache instead"))
          files <- list.files(path=cache_folder, pattern =paste0(code,'_',year,'.rds',collapse="|"), full.names = T)
          tryCatch({
            files %>% purrr::map_dfr(readFile) %>% bind_rows()
          }, error=function(c){
            print("DEBUG: ERROR IN importNOAA")
            NULL
          })
        })
    }

    # Downloading fresh data
    if(length(years_to_download)>0){
      data_downloaded <- do.call("bind_rows",
                                 lapply(years_to_download,
                                        download_data,
                                        code=code,
                                        cache_folder=cache_folder))
    }else{
      data_downloaded <- NULL
    }
    
    # Binding and returning
    result <- bind_rows(data_cached, data_downloaded)
    if(nrow(result)==0){return(NULL)}
    
    # Aggregate per day
    # Note: Certain dates have no data at all and hence raise warnings
    suppressWarnings(result <- result %>%
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
      ) %>%
      # Replace infinite with NA
      mutate_at(vars(-date), ~ifelse(is.infinite(.x), NA, .x)))
    
    result
  }, error=function(err){
    message(err)
    warning(err)
    return(NULL)}
  )
}


noaa.get_folder <- function(){
  dir_noaa <- Sys.getenv("DIR_NOAA_ISD")
  dir.create(dir_noaa, showWarnings = F, recursive = T)
  if(!dir.exists(dir_noaa)){
    stop("Failed to read/create DIR_NOAA_ISD")
  }
  dir_noaa
}


noaa.collect_weather <- function(locations_w_stations,
                                 weather_vars,
                                 years_force_refresh=year(today())){
  
  print("Adding weather from NOAA")
  cache_folder <- noaa.get_folder()
  
  weather <- locations_w_stations %>%
    dplyr::ungroup() %>%
    as.data.frame() %>%
    rowwise() %>%
    mutate(date_from=min(weather$date, na.rm=T),
           date_to=max(weather$date, na.rm=T)) %>%
    tidyr::unnest(cols=(noaa_station)) %>%
    dplyr::distinct(location_id, usaf, wban, date_from, date_to, weather)
  
  weather$code <- paste(weather$usaf, weather$wban, sep="-")
  # print(paste("Codes:", paste(unique(stations_weather$code)), collapse=","))
  
  weather$weather_noaa <- tryCatch({
    pbapply::pbmapply(noaa.get_noaa_at_code,
                      code=weather$code,
                      year_from=lubridate::year(weather$date_from),
                      year_to=lubridate::year(weather$date_to),
                      years_force_refresh=list(years_force_refresh),
                      cache_folder=cache_folder,
                      SIMPLIFY=F)  
  }, error=function(err){
    warning(err)
    return(NA)
  })
  

  to_date <- function(d){
    tryCatch({
      lubridate::date(d)
    },error=function(err){
      print(d)
      NA
    })
  }
  
  if(nrow(weather %>% rowwise() %>% filter(!is.null(weather_noaa)))==0){
    stop("Failed to find weather data")
  }
  
  # Note: Certain dates have no data at all and hence raise warnings
  locations_weather <- weather %>%
    unnest(weather_noaa) %>%
    group_by(location_id, weather) %>%
    mutate(date=to_date(date)) %>%
    filter(!is.na(date)) %>%
    filter(date>=unique(date_from)) %>%
    filter(date<=unique(date_to)) %>%
    group_by(location_id, weather, date) %>%
    summarize(
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
    ) %>%
    select(c('location_id', 'weather', 'date',
             intersect(names(.), weather_vars))) %>%
    group_by(location_id, weather) %>%
    tidyr::nest() %>%
    dplyr::rowwise() %>%
    # Only keep required dates
    mutate(weather=list(weather %>%
                          left_join(data %>%
                                      select(c('date', intersect(names(data), weather_vars))),
                                    by="date"))) %>%
    select(-c(data)) %>%
    ungroup()
  
  return(locations_weather)
}


