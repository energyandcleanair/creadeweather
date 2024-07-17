get_weather <- function(location_ids,
                        date_from,
                        date_to,
                        weather_vars,
                        weather_sources = c("era5"),
                        read_weather_filename = NULL,
                        save_weather_filename = NULL,
                        n_per_location = 4,
                        years = seq(2015, 2020),
                        years_force_refresh = NULL,
                        add_pbl = T,
                        add_sunshine = T,
                        add_fire = F,
                        fire_source = "viirs",
                        fire_mode = "oriented",
                        fire_buffer_km = NULL,
                        fire_split_days = F,
                        fire_split_regions = NULL,
                        fire_split_regions_res = "low",
                        trajs_parallel = T,
                        trajs_cores = parallel::detectCores() - 1,
                        trajs_height = NULL,
                        trajs_hours = seq(0, 23, 4),
                        trajs_duration_hour = 72,
                        trajs_met_type = "gdas1",
                        use_trajs_cache = T,
                        use_weather_cache = T,
                        upload_trajs = F,
                        upload_weather = F,
                        save_trajs_filename = NULL) {
  
  
  if (!is.null(read_weather_filename) && file.exists(read_weather_filename)) {
    weather <- read_weather(read_weather_filename)
  } else {
    weather <- collect_weather(
      location_ids = location_ids,
      date_from = date_from,
      date_to = date_to,
      weather_vars = weather_vars,
      weather_sources = weather_sources,
      years = years,
      years_force_refresh = years_force_refresh,
      n_per_location = n_per_location,
      add_sunshine = F,
      add_fire = add_fire,
      fire_source = fire_source,
      fire_mode = fire_mode,
      fire_buffer_km = fire_buffer_km,
      fire_split_days = fire_split_days,
      fire_split_regions = fire_split_regions,
      fire_split_regions_res = fire_split_regions_res,
      trajs_parallel = trajs_parallel,
      trajs_cores = trajs_cores,
      trajs_height = trajs_height,
      trajs_duration_hour = trajs_duration_hour,
      trajs_hours = trajs_hours,
      trajs_met_type = trajs_met_type,
      use_trajs_cache = use_trajs_cache,
      use_weather_cache = use_weather_cache,
      upload_trajs = upload_trajs,
      upload_weather = upload_weather,
      save_trajs_filename = save_trajs_filename
    )
    if (!is.null(save_weather_filename)) {
      saveRDS(weather, save_weather_filename)
    }
  }

  return(weather)
}

#' Collect weather (and atmospheric) data from various sources
#'
#' @param location_ids
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
collect_weather <- function(location_ids,
                            date_from,
                            date_to,
                            weather_vars,
                            weather_sources = c("era5", "noaa"),
                            n_per_location = 4,
                            years = seq(2015, 2020),
                            years_force_refresh = year(today()),
                            add_pbl = T,
                            add_sunshine = T,
                            add_fire = F,
                            fire_source = "viirs",
                            fire_mode = "trajectory",
                            fire_buffer_km = NULL,
                            fire_split_days = F,
                            fire_split_regions = NULL,
                            fire_split_regions_res = "low",
                            trajs_parallel = T,
                            trajs_cores = parallel::detectCores() - 1,
                            trajs_hours = seq(0, 23, 4),
                            trajs_duration_hour = 120,
                            trajs_height = NULL,
                            trajs_met_type = "gdas1",
                            use_trajs_cache = T,
                            use_weather_cache = T,
                            upload_trajs = F,
                            upload_weather = F,
                            save_trajs_filename = NULL) {
  
  if (is.null(location_ids)) {
    stop("missing location_ids")
  }

  location_dates <- rcrea::locations(id = location_ids, with_source = F) %>%
    distinct(location_id = id, country, geometry) %>%
    mutate(date_from = date_from, date_to = date_to)

  if (nrow(location_dates) == 0) {
    stop("no valid location_id found")
  }
  
  weather_noaa <- NULL
  weather_era5 <- NULL
  weather_sirad <- NULL

  if ("noaa" %in% weather_sources) {
    weather_noaa <- noaa.collect_weather(
      location_dates = location_dates,
      weather_vars = weather_vars,
      n_per_location = n_per_location,
      years_force_refresh = years_force_refresh
    )
  }

  if ("era5" %in% weather_sources) {
    weather_era5 <- era5.collect_weather(
      location_dates = location_dates,
      weather_vars = weather_vars
    )
  }
  
  if (add_sunshine){
    weather_vars <- unique(c(weather_vars, "sunshine"))
    weather_sources <- unique(c(weather_sources, "sirad"))
    weather_sirad <- sirad.collect_weather(
      location_dates = location_dates
      )
  }

  weather <- combine_weathers(
    weathers = list(
      "noaa" = weather_noaa,
      "era5" = weather_era5,
      "sirad" = weather_sirad
    ),
    weather_sources = weather_sources
  )

  # Add fire radiative power
  if (add_fire) {
    print("Getting Fire Radiative Power (will compute trajectories if required)")
    weather <- fire.add_fire(weather,
      source = fire_source,
      mode = fire_mode,
      duration_hour = trajs_duration_hour,
      met_type = trajs_met_type,
      buffer_km = fire_buffer_km,
      trajs_hours = trajs_hours,
      trajs_height = trajs_height,
      trajs_parallel = trajs_parallel,
      trajs_cores = trajs_cores,
      split_days = fire_split_days,
      split_regions = fire_split_regions,
      split_regions_res = fire_split_regions_res,
      use_trajs_cache = use_trajs_cache,
      upload_trajs = upload_trajs,
      upload_weather = upload_weather,
      use_weather_cache = use_weather_cache,
      save_trajs_filename = save_trajs_filename
    )
  }

  return(weather)
}


available_weather_vars <- function(weather){
  setdiff(names(ungroup(weather) %>% select(weather) %>% unnest(weather)), "date")
}


#' Combine weather from different sources by selecting
#' for each variable the first one in weather_sources
#' that has at least min_share_available available
#'
#' @param weathers 
#' @param weather_sources 
#' @param min_pct 
#'
#' @return
#' @export
#'
#' @examples
combine_weathers <- function(
    weathers,
    weather_sources=names(weathers),
    min_share_available = 0.5) {
  
  w <- lapply(weather_sources, function(weather_source) {
    w <- weathers[[weather_source]]
    if (is.null(w)) {
      return(NULL)
    }
    w %>%
      select(location_id, weather) %>%
      tidyr::unnest(weather) %>%
      tidyr::gather(key = "weather_var", value = "value", -c(location_id, date)) %>%
      mutate(source = weather_source)
  }) %>%
    bind_rows()

  selected <- w %>%
    ungroup() %>%
    tidyr::complete(location_id, date, tidyr::nesting(weather_var, source),
                    fill=list(value=NA)) %>%
    group_by(location_id, weather_var, source) %>%
    summarise(n_nonna = sum(!is.na(value)),
              n_total=n(),
              share_available = n_nonna / n_total) %>%
  # Arrange based on weather_sources
  mutate(source = factor(source, levels = weather_sources, ordered=T)) %>%
  # Pick first one above min_pct
  filter(share_available >= min_share_available) %>%
  select(-c(n_nonna, n_total, share_available)) %>%
  arrange(source) %>%
  dplyr::slice_head(n=1)
  
  selected %>%
     left_join(w) %>%
    select(-c(source)) %>%
    tidyr::spread(weather_var, value) %>%
    group_by(location_id) %>%
    tidyr::nest() %>%
    rename(weather=data)
}


combine_meas_weather <- function(meas, weather) {
  print("Attaching weather to measurements")
  meas %>%
    tibble() %>%
    select(setdiff(names(.), "geometry")) %>%
    dplyr::left_join(
      weather %>%
        tibble() %>%
        select(location_id, weather),
      by = "location_id"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::filter(
      !is.null(weather),
      !is.null(meas)
    ) %>%
    dplyr::mutate(meas = list(
      meas %>%
        dplyr::mutate(date = date(date)) %>%
        dplyr::left_join(weather, by = "date")
    )) %>%
    dplyr::rename(meas_weather = meas) %>%
    dplyr::select(-c(weather))
}


read_weather <- function(filename) {
  readRDS(filename) %>%
    select_at(setdiff(names(.), "value"))
}
