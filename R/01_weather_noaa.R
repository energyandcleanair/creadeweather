

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
    dplyr::left_join(as.data.frame(meas_stations  %>% dplyr::select(-c(geometry))))
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
  years <- as.numeric(stringr::str_extract(files, "(?<=_)\\d{4}(?=\\.rds)"))
  # Only keep files who have been updated after the end of the year
  # But also keep those who have been updated today
  is_valid <- file.info(files)$mtime >= pmin(as.POSIXct(paste0(years+1,"-01-01")),
                                            lubridate::today())
  return(years[is_valid])
}

noaa.get_noaa_at_code <- function(code, years, years_force_refresh=NULL, cache_folder){
  # Get NOAA data, first trying to use cached files for complete years
  tryCatch({
    # Reading cache files
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
            print(c)
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

noaa.get_folder <- function(){
  dir_noaa <- Sys.getenv("DIR_NOAA_ISD")
  dir.create(dir_noaa, showWarnings = F, recursive = T)
  if(!dir.exists(dir_noaa)){
    stop("Failed to read/create DIR_NOAA_ISD")
  }
  dir_noaa
}

noaa.add_weather <- function(meas_w_stations, years=c(2015:2020), years_force_refresh=c(2020)){
  print("Adding weather from NOAA")
  cache_folder <- noaa.get_folder()
  
  stations_weather <- meas_w_stations %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols=(noaa_station)) %>%
    dplyr::distinct(station_id, usaf, wban)
  
  stations_weather$code <- paste(stations_weather$usaf, stations_weather$wban, sep="-")
  print(paste("Codes:", paste(unique(stations_weather$code)), collapse=","))
  
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
    dplyr::left_join(
      stations_weather %>%
        as.data.frame() %>%
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
  print("Done [Adding weather from NOAA]")
  return(meas_w_stations)
}


