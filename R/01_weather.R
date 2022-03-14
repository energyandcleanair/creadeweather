#' Collect weather (and atmospheric) data from various sources including NOAA & UNCAR.
#'
#' @param meas tibble of measurements with date and geometry
#' @param n_per_station how many NOAA stations do we fetch per AQ measurement station (default 2)
#' @param years which years are we getting measurements from
#' @param years_force_refresh ignoring cache files for these years
#' @param add_pbl Adding planet boundary layer (not ready)
#' @param add_sunshine Adding sunshine information
#' @param trajs_height receptor height for trajectories in meter.
#' If null, pbl average will be considered.
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
                            add_fire=F,
                            fire_source="viirs",
                            fire_mode="oriented",
                            fire_duration_hour=72,
                            fire_buffer_km=NULL,
                            fire_split_days=F,
                            fire_split_regions=NULL,
                            trajs_parallel=T,
                            trajs_height=NULL,
                            use_trajs_cache=T,
                            save_trajs_filename=NULL){
    
  if("date" %in% colnames(meas) | !"meas" %in% colnames(meas)){
    stop("Measurements should be nested in meas column")
  }

  # Find unique AQ stations
  stations <- meas %>%
    as.data.frame() %>%
    dplyr::select(country, location_id, meas) %>%
    tidyr::unnest(meas) %>%
    dplyr::distinct(country, location_id, timezone, date) %>%
    dplyr::group_by(country, location_id, timezone) %>%
    tidyr::nest() %>%
    rename(dates=data) %>%
    left_join(meas %>% ungroup() %>% distinct(location_id, geometry)) %>%
    sf::st_as_sf() %>%
    ungroup()
  
    

  # Find weather stations nearby
  stations_w_noaa <- suppressMessages(
    noaa.add_close_stations(stations, n_per_station = n_per_station))

  # Get weather at these stations
  weather <- noaa.add_weather(stations_w_noaa,
                              years_force_refresh = years_force_refresh) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.null(weather))
  
  # Add Planet Boundary Layer from NCAR
  if(add_pbl){
    print("Getting Planet Boundary Layer")
    weather <- cfs.add_pbl(weather)  
  }
  
  # Add sunshine
  if(add_sunshine){
    print("Getting Sunshine")
    weather <- sirad.add_sunshine(weather)  
  }
  
  # Add fire radiative power
  if(add_fire){
    print("Getting Fire Radiative Power (will compute trajectories if required)")
    weather <- fire.add_fire(weather,
                           source=fire_source,
                           mode=fire_mode,
                           duration_hour=fire_duration_hour,
                           buffer_km=fire_buffer_km,
                           trajs_height=trajs_height,
                           trajs_parallel=trajs_parallel,
                           split_days=fire_split_days,
                           split_regions=fire_split_regions,
                           use_trajs_cache=use_trajs_cache,
                           save_trajs_filename=save_trajs_filename)  
  }
  
  # Unnest
  weather <- weather %>%
    as.data.frame() %>%
    select(-c(noaa_station, dates)) %>%
    tidyr::unnest(weather)
  
  return(weather)
}


combine_meas_weather <- function(meas, weather){
  
  # Join weather with measurements
  print("Attaching weather to measurements")
  if("geometry" %in% colnames(meas)){
    meas <- tibble(meas) %>% dplyr::select(-c(geometry))
  }else{
    meas <- tibble(meas)
  }
  
  weather_nested <- weather %>%
    group_by(location_id) %>%
    tidyr::nest() %>%
    rename(weather=data)
  
  meas_w_weather <- tibble(meas) %>%
    dplyr::left_join(weather_nested,
                     by="location_id") %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(weather)) %>%
    dplyr::filter(!is.null(meas)) %>%
    dplyr::mutate(meas=list(
      meas %>%
        dplyr::mutate(date=lubridate::date(date)) %>%
        dplyr::left_join(weather, by="date")
    )) %>%
    dplyr::rename(meas_weather=meas) %>%
    dplyr::select(-c(weather))
  
  return(meas_w_weather)
}

read_weather <- function(filename){
  readRDS(filename) %>%
    # To accomodate for older versions that had one per date AND pollutant
    distinct(location_id, date, .keep_all=T) %>%
    select_at(setdiff(names(.), "value"))
}

