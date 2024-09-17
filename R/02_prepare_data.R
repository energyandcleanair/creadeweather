#' Prepare data before training. Includes:
#' - cleaning
#' - filling missing values
#' - adding variables (e.g. lag variables, wind sector)
#' - filtering

#'
#' @param meas_weather
#'
#' @return
#' @export
#'
#' @examples
prep_data <- function(data,
                      weather_vars,
                      time_vars,
                      lag=0){
  
  prep_meas_weather <- function(meas_weather){
      tryCatch({
        meas_weather %>%
          clean_data() %>%
          fill_data() %>%
          enrich_data(lag=lag,
                      weather_vars=weather_vars) %>%
          filter_data(weather_vars=weather_vars) %>%
          utils.add_timevars(add=time_vars)
      }, error = function(e){
        return(NULL)
      })
  }
  
  data <- data %>%
    dplyr::rowwise() %>%
    mutate(meas_weather=list(prep_meas_weather(meas_weather))) %>%
    ungroup()
  
  # Remove empty configurations
  data <- data[unlist(sapply(data$meas_weather, length)) > 0, ]
 
  return(data)
}


# Filter outliers etc.
clean_data <- function(tbl){
  
  # Remove infs in all vars
  tbl <- tbl %>% dplyr::mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x))
  
  # Apply mad filtering to pollutant peaks (very useful for e.g. sand storms in Beijing)
  n_mad <- 10
  mad <- stats::mad(tbl$value, na.rm=T)
  n_before <- nrow(tbl)
  
  tbl <- tbl %>% dplyr::filter(value-median(value, na.rm=T) < n_mad * mad)
  n_after <- nrow(tbl)
  if(n_before!=n_after){
    print(paste(n_before-n_after,"/", n_before,"outliers removed. Threshold:", n_mad*mad+median(tbl$value)))
  }
  
  # Replace NaNs with NA (gbm doesn't like NaNs)
  tbl <- tbl %>%
    utils.replace_nan_with_na() %>%
    filter(!is.na(date))
  
  return(tbl)
}


# Certain stations miss certain weather variables
fill_data <- function(tbl){
  
  weather_cols <- setdiff(names(tbl), c("date", "timezone", "value"))
  
  # Different kind of treatments
  
  # Treatment 1: If all NA, then replace with 0
  # We do it for all variables
  vars_allnatozero <- weather_cols
  allnatozero <- function(v){
    if(all(is.na(v))) v[is.na(v)] <- 0
    v
  }
  
  tbl <- tbl %>% mutate_at(intersect(vars_allnatozero, weather_cols),
                           allnatozero)
  
  # Treatment 2: Replace NA with 0
  # We do it only for precipitation and fire
  vars_natozero <- c("precip", grep("fire_", names(tbl), value=T))
  natozero <- function(v){
    v[is.na(v)] <- 0
    v
  }
  tbl <- tbl %>% mutate_at(intersect(vars_natozero, weather_cols),
                           natozero)
  
  
  # Treatment 3: interpolate missing values
  max_gap = 5
  vars_interpolate <- c("sunshine", "atmos_pres", "air_temp_min", "air_temp_max",
                        "air_temp", "ceil_hgt", "pbl_min", "pbl_max", "dewpoint_temp")
  interpolate <- function(v, date){
    zoo::na.approx(v, date, na.rm=FALSE, maxgap=max_gap)
  }
  
  tbl <- tbl %>% mutate_at(intersect(vars_interpolate, weather_cols),
                           ~interpolate(.x,date=date))
  
  return(tbl)
}



# Add lag and wind direction factor
enrich_data <- function(tbl, lag, weather_vars){
  
  tbl$wd_factor <- factor(tbl$wd %/% 45)
  
  # Add lag if need be
  if(!is.null(lag) & lag>0){
    day_lags <- c(1:lag)  
    tbl <- utils.add_lag(tbl, weather_vars, group_cols=c(), day_lags, 'day')
  }
  
  return(tbl)
}



filter_data <- function(tbl, weather_vars){
  
  # Only keep if recent data exists
  if(max(tbl$date, na.rm=T)<'2020-01-01'){
    warning("No measurements available since 2020. Returning NA")
    return(NA)
  }
  
  # With original weather data (lag doesn't count)
  tbl <- tbl %>%
    dplyr::filter(if_all(weather_vars, ~ !is.na(.)))
  
  return(tbl)
}
