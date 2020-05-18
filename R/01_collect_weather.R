#' Collect weather (and atmospheric) data from various sources including NOAA & UNCAR.
#'
#' @param meas tibble of measurements with date and geometry
#' @param pollutants 
#' @param deg Only used for filename... #TODO clean
#' @param n_per_station how many NOAA stations do we fetch per AQ measurement station (default 2)
#' @param years_force_refresh 
#'
#' @return
#' @export
#'
#' @examples
collect_weather <- function(meas,
                            pollutants,
                            n_per_station=2,
                            years=seq(2015,2020),
                            years_force_refresh=NULL,
                            add_pbl=T,
                            add_sunshine=T,
                            filename){
    
  cache_folder <- file.path('data', '01_weather', 'cache')
  if(!dir.exists(cache_folder)) dir.create(cache_folder, recursive = T)
  
  output_folder <- file.path('data', '01_weather', 'output')
  if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
  
  input_folder <- file.path('data', '01_weather', 'input')
  if(!dir.exists(input_folder)) dir.create(input_folder, recursive = T)
  
  print("Getting NOAA")
  stations <- meas %>% ungroup() %>% dplyr::select(station_id, geometry) %>% distinct(station_id, .keep_all = T)
  stations_w_noaa <- noaa.add_close_stations(stations, n_per_station = n_per_station)
  weather <- noaa.add_weather(stations_w_noaa, years=years,
                                     years_force_refresh = years_force_refresh,
                                     cache_folder = cache_folder)
  
  # Add Planet Boundary Layer from NCAR
  if(add_pbl){
    print("Getting Planet Boundary Layer")
    weather <- ncar.add_pbl(weather, years)  
  }
  
  # Add sunshine
  if(add_sunshine){
    print("Getting Sunshine")
    weather <- sirad.add_sunshine(weather)  
  }
  
  # Join weather into measurements
  if("geometry" %in% colnames(meas)){
    meas <- tibble(meas) %>% dplyr::select(-c(geometry))
  }else{
    meas <- tibble(meas)
  }
  
  meas_w_weather <- tibble(meas) %>%
    left_join(weather %>% dplyr::select(station_id, weather)) %>%
    rowwise() %>%
    dplyr::filter(!is.null(weather)) %>%
    dplyr::filter(!is.null(meas)) %>%
    mutate(meas=list(
      meas %>% left_join(weather)
    )) %>%
    rename(meas_weather=meas) %>%
    dplyr::select(-c(weather))
 
  saveRDS(meas_w_weather, file.path(output_folder, filename))
  return(meas_w_weather)
}

