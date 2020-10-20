#' Collect weather (and atmospheric) data from various sources including NOAA & UNCAR.
#'
#' @param meas tibble of measurements with date and geometry
#' @param n_per_station how many NOAA stations do we fetch per AQ measurement station (default 2)
#' @param years which years are we getting measurements from
#' @param years_force_refresh ignoring cache files for these years
#' @param add_pbl Adding planet boundary layer (not ready)
#' @param add_sunshine Adding sunshine information
#' @param filename If not null, results are saved under filename in the output folder
#'
#' @return
#' @export
#'
#' @examples
#' 
collect_weather <- function(meas,
                            n_per_station=2,
                            years=seq(2015,2020),
                            years_force_refresh=NULL,
                            add_pbl=T,
                            add_sunshine=T,
                            add_frp=F, #Fire radiative potential (will compute trajectories)
                            filename=NULL){
    
  if("date" %in% colnames(meas) | !"meas" %in% colnames(meas)){
    stop("Measurements should be nested in meas column")
  }

  cache_folder <- utils.get_cache_folder("weather")

  # Find unique AQ stations
  stations <- meas %>%
    dplyr::ungroup() %>%
    dplyr::select(country, station_id, geometry) %>%
    dplyr::distinct(country, station_id, .keep_all = T)

  # Find weather stations nearby
  stations_w_noaa <- noaa.add_close_stations(stations, n_per_station = n_per_station)

  # Get weather at these stations
  weather <- noaa.add_weather(stations_w_noaa, years=years,
                                     years_force_refresh = years_force_refresh,
                                     cache_folder = cache_folder) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.null(weather))
  
  # Add Planet Boundary Layer from NCAR
  if(add_pbl){
    print("Getting Planet Boundary Layer")
    weather <- ncep.add_pbl(weather, years)  
  }
  
  # Add sunshine
  if(add_sunshine){
    print("Getting Sunshine")
    weather <- sirad.add_sunshine(weather)  
  }
  
  # Add fire radiative power
  if(add_frp){
    print("Getting Fire Radiative Power (will compute trajectories if required)")
    weather <- frp.add_frp(weather)  
  }
  
  # Join weather with measurements
  print("Attaching weather to measurements")
  if("geometry" %in% colnames(meas)){
    meas <- tibble(meas) %>% dplyr::select(-c(geometry))
  }else{
    meas <- tibble(meas)
  }
  
  meas_w_weather <- tibble(meas) %>%
    dplyr::left_join(weather %>% dplyr::select(station_id, weather)) %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(weather)) %>%
    dplyr::filter(!is.null(meas)) %>%
    dplyr::mutate(meas=list(
      meas %>%
        dplyr::mutate(date=lubridate::date(date)) %>%
        dplyr::left_join(weather)
    )) %>%
    dplyr::rename(meas_weather=meas) %>%
    dplyr::select(-c(weather))
 
  if(!is.null(filename)){
    ouput_folder <- utils.get_output_folder()
    saveRDS(meas_w_weather, file.path(output_folder, filename))
  }
  return(meas_w_weather)
}

