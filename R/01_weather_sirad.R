sirad.collect_weather <- function(location_dates) {
  
  locations_sf <- st_as_sf(location_dates %>% distinct(location_id, geometry))

  # Get hourly solar radiation at each day and hour
  sunshine <- locations_sf %>%
    tidyr::crossing(doy = seq(1:365)) %>%
    mutate(longitude = purrr::map_dbl(geometry, ~ sf::st_coordinates(.x)[[1]])) %>%
    mutate(latitude = purrr::map_dbl(geometry, ~ sf::st_coordinates(.x)[[2]])) %>%
    mutate(
      sunshine = purrr::map2_dbl(
        doy, 
        latitude, 
        ~ sirad::extrat(.x, sirad::radians(.y))$ExtraTerrestrialSolarRadiationDaily
      )
    )
  
  location_dates %>%
    group_by(location_id) %>%
    mutate(date = list(seq.Date(date(date_from), date(date_to), by = "1 day"))) %>%
    tidyr::unnest(date) %>%
    mutate(doy = lubridate::yday(date)) %>%
    left_join(sunshine) %>%
    select(location_id, date, sunshine) %>%
    tidyr::nest(weather = -c(location_id))
}
