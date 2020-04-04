library(raster)
library(sf)
library(dplyr)
library(stringr)

input_folder <- file.path('data', '01_weather', 'input')

ncar.add_pbl <- function(meas_w_weather){
  stations_sf <- st_as_sf(meas_w_weather %>% ungroup() %>%
                            dplyr::select(station_id, geometry) %>%
                            dplyr::distinct(station_id, .keep_all=T))
  
  
  folder <- file.path(input_folder, 'pbl')
  files <- list.files(folder, "*.grb2", full.names = T)
  pat = 'cdas1\\.(\\d{8})\\.pgrbh'
  dates <- lubridate::ymd(str_match(files, pat)[, 2])
 
  process_file <- function(stations_sf, file){
    pat = 'cdas1\\.(\\d{8})\\.pgrbh'
    date <- lubridate::ymd(str_match(file, pat)[, 2])
    r <- raster::brick(file)
    file_values <- as.data.frame(raster::extract(r,
                                       stations_sf,
                                       # buffer=500, # 500m radius
                                       fun=mean,na.rm=T,
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
  pbl_values <- do.call('rbind', pblapply(files, process_file, stations_sf=stations_sf))
  
  # Join to weather data
  joined <- meas_w_weather %>% rowwise() %>%
    mutate(weather_station_id=station_id, weather=list(weather %>% left_join(
      pbl_values %>% filter(station_id==weather_station_id)
    )))
  
  return(joined)
}


