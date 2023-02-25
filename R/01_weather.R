get_weather <- function(meas,
                        weather_vars,
                        read_weather_filename=NULL,
                        save_weather_filename=NULL,
                        n_per_location=4,
                        years=seq(2015, 2020),
                        years_force_refresh=NULL,
                        add_pbl=T,
                        add_sunshine=T,
                        add_fire=F,
                        fire_source="viirs",
                        fire_mode="oriented",
                        fire_buffer_km=NULL,
                        fire_split_days=F,
                        fire_split_regions=NULL,
                        trajs_parallel=T,
                        trajs_height=NULL,
                        trajs_hours=seq(0,23,4),
                        trajs_duration_hour=72,
                        trajs_met_type='gdas1',
                        use_trajs_cache=T,
                        upload_trajs=F,
                        upload_weather=F,
                        save_trajs_filename=NULL){
  
  if(!is.null(read_weather_filename) && file.exists(read_weather_filename)){
    weather <- read_weather(read_weather_filename)
  }else{
    weather <- collect_weather(meas,
                               weather_vars=weather_vars,
                               years=years,
                               years_force_refresh=years_force_refresh,
                               n_per_location=n_per_location,
                               add_sunshine=F,
                               add_fire=add_fire,
                               fire_source=fire_source,
                               fire_mode=fire_mode,
                               fire_buffer_km=fire_buffer_km,
                               fire_split_days=fire_split_days,
                               fire_split_regions=fire_split_regions,
                               trajs_parallel=trajs_parallel,
                               trajs_height=trajs_height,
                               trajs_duration_hour=trajs_duration_hour,
                               trajs_hours=trajs_hours,
                               trajs_met_type=trajs_met_type,
                               use_trajs_cache=use_trajs_cache,
                               upload_trajs=upload_trajs,
                               upload_weather=upload_weather,
                               save_trajs_filename=save_trajs_filename
    )
    if(!is.null(save_weather_filename)){
      saveRDS(weather, save_weather_filename)
    }
  }
  
  return(weather)
}

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
                            weather_vars,
                            n_per_location=4,
                            years=seq(2015,2020),
                            years_force_refresh=year(today()),
                            add_pbl=T,
                            add_sunshine=T,
                            add_fire=F,
                            fire_source="viirs",
                            fire_mode="oriented",
                            fire_buffer_km=NULL,
                            fire_split_days=F,
                            fire_split_regions=NULL,
                            trajs_parallel=T,
                            trajs_hours=seq(0,23,4),
                            trajs_duration_hour=72,
                            trajs_height=NULL,
                            trajs_met_type='gdas1',
                            use_trajs_cache=T,
                            upload_trajs=F,
                            upload_weather=F,
                            save_trajs_filename=NULL){
    
  
  if(!all(c('meas','location_id','timezone','geometry') %in% names(meas))){
    stop("meas is missing columns")
  }

  # Get unique location/date combinations
  # e.g. some locations may gave different process_id
  weather <- meas %>%
    as.data.frame() %>%
    dplyr::select(country, location_id, meas, geometry) %>%
    tidyr::unnest(meas) %>%
    dplyr::distinct(country, location_id, timezone, date) %>%
    dplyr::group_by(country, location_id, timezone) %>%
    tidyr::nest() %>%
    rename(weather=data) %>%
    left_join(meas %>% ungroup() %>% distinct(location_id, geometry)) %>%
    sf::st_as_sf() %>%
    ungroup()
  
  
  
  weather <- noaa.add_weather(location_dates=weather,
                              weather_vars=weather_vars,
                              n_per_location=n_per_location,
                              years_force_refresh=years_force_refresh)
  
  
  if(any(grepl('pbl', weather_vars))){
    weather <- era5.add_weather(weather, weather_vars=grep('pbl', weather_vars, value = T))
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
                           duration_hour=trajs_duration_hour,
                           met_type=trajs_met_type,
                           buffer_km=fire_buffer_km,
                           trajs_hours=trajs_hours,
                           trajs_height=trajs_height,
                           trajs_parallel=trajs_parallel,
                           split_days=fire_split_days,
                           split_regions=fire_split_regions,
                           use_trajs_cache=use_trajs_cache,
                           upload_trajs=upload_trajs,
                           upload_weather=upload_weather,
                           save_trajs_filename=save_trajs_filename)
  }

  return(weather)
}


combine_meas_weather <- function(meas, weather){
  
  print("Attaching weather to measurements")
  meas %>%
    tibble() %>%
    select(setdiff(names(.), 'geometry')) %>%
    dplyr::left_join(weather %>%
                       tibble() %>%
                       select(location_id, weather),
                     by='location_id') %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(weather),
                  !is.null(meas)) %>%
    dplyr::mutate(meas=list(
      meas %>%
        dplyr::mutate(date=date(date)) %>%
        dplyr::left_join(weather, by="date")
    )) %>%
    dplyr::rename(meas_weather=meas) %>%
    dplyr::select(-c(weather))
}

read_weather <- function(filename){
  readRDS(filename) %>%
    select_at(setdiff(names(.), "value"))
}

