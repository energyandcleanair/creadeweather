input_folder <- file.path('data', '01_weather', 'input')

ncar.add_pbl <- function(weather){
  stations_sf <- st_as_sf(weather %>% ungroup() %>%
                            dplyr::select(station_id, geometry) %>%
                            dplyr::distinct(station_id, .keep_all=T))
  
  
  folder <- file.path(input_folder, 'pbl')
  files <- list.files(folder, "*.grb2$", full.names = T)
  pat = 'cdas1\\.(\\d{8})\\.'
  dates <- lubridate::ymd(stringr::str_match(files, pat)[, 2])
 
  process_file <- function(file, stations_sf){
    pat = 'cdas1\\.(\\d{8})\\.'
    date <- lubridate::ymd(stringr::str_match(file, pat)[, 2])
    r <- raster::brick(file)
    # For some reason, on GCP/Ubuntu, raster is offset by 360deg
    if(raster::xmin(r)>180){
      raster::xmin(r)=raster::xmin(r)-360
      raster::xmax(r)=raster::xmax(r)-360
    }
    file_values <- as.data.frame(raster::extract(r,
                                       stations_sf,
                                       # buffer=500, # 500m radius
                                       fun=mean,
                                       na.rm=T,
                                       sp = TRUE,
                                       method='simple')) %>%
      dplyr::select(-c(coords.x1, coords.x2)) %>%
      tidyr::gather("key","pbl",-station_id) %>%
      group_by(station_id) %>%
      summarize(pbl_min=min(pbl, na.rm=T),
                pbl_max=max(pbl, na.rm=T)) %>%
      mutate(date=date)

    return(file_values)
  }
  pbl_values <- do.call('bind_rows', pblapply(files, process_file, stations_sf=stations_sf))
  
  # Join to weather data
  joined <- weather %>% rowwise() %>%
    mutate(weather_station_id=station_id, weather=list(weather %>% left_join(
      pbl_values %>% filter(station_id==weather_station_id)
    ))) %>% dplyr::select(-c(weather_station_id))
  
  return(joined)
}


