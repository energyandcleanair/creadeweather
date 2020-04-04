require(purrr)
require(sf)
require(dplyr)
require(tidyr)
require(pbapply)

if(!require(install.load)) install.packages("install.load"); require(install.load)
install_load('sirad')

sirad.add_sunshine <- function(meas_w_weather){
  
  stations_sf <- st_as_sf(meas_w_weather %>% ungroup() %>%
                            dplyr::select(station_id, geometry) %>%
                            dplyr::distinct(station_id, .keep_all=T))
  

  # Get hourly solar radiation at each day and hour
  sunshine <- stations_sf %>%
    tidyr::crossing(doy=seq(1:365)) %>%
    mutate(longitude=purrr::map_dbl(geometry, ~sf::st_coordinates(.x)[[1]])) %>%
    mutate(latitude=purrr::map_dbl(geometry, ~sf::st_coordinates(.x)[[2]])) %>%
    mutate(sunshine=purrr::map2_dbl(doy, latitude, ~sirad::extrat(.x, sirad::radians(.y))$ExtraTerrestrialSolarRadiationDaily))

  sunshine_nested <- sunshine %>%
    tidyr::nest(sunshine=c(doy, sunshine)) %>%
    dplyr::select(station_id, sunshine)

  # Merge with previous weather data
  w_sunshine <- meas_w_weather %>%
    left_join(sunshine_nested) %>%
    rowwise() %>%
    mutate(weather= list(
             weather %>% mutate(doy=lubridate::yday(date)) %>%
             left_join(sunshine) %>% dplyr::select(-c(doy))
           )) %>%
    dplyr::select(-c(sunshine))

  return(w_sunshine)
}