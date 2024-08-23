#' A function that computes y-o-y changes and attributes them to either change in weather
#' or emissions
#'
#' It basically runs deweathering while excluding each month and the previous one
#' from training, and computing fchanges
#'
#' @return
#' @export
#'
#' @examples
deweather_yoy <- function(
    months,
    upload_results,
    deweather_process_id,
    save_weather_filename = NULL,
    read_weather_filename = NULL,
    ...) {
  
  # One weather file for all 
  keep_weather_file <- T
  
  if(is.null(save_weather_filename)){
    save_weather_filename <- tempfile(fileext = ".RDS")  
    keep_weather_file <- F
  }
  if(is.null(read_weather_filename)){
    read_weather_filename <- save_weather_filename
  }

  
  deweathered_yoys <- lapply(months, function(month) {
    # Exclude month dates and the previous year from training
    dates <- get_excluded_yoy_dates(month)
    
    deweathered <- creadeweather::deweather(
      ...,
      deweather_process_id=deweather_process_id,
      upload_results = F, # We'll upload after if need be
      training_excluded_dates = dates,
      save_weather_filename = save_weather_filename,
      read_weather_filename = read_weather_filename
    )
    
    deweathered_yoy <- extract_yoy_changes(deweathered, month)
    
    if(upload_results){
      creadeweather::upload_results(results=deweathered_yoy,
                                    deweather_process_id=deweather_process_id)
    }
    
    return(deweathered_yoy)
  }) %>%
    bind_rows()
  
  
  if(!keep_weather_file) file.remove(save_weather_filename)

  return(deweathered_yoys)
}


#' For each config, add yoy and yoy_rel variables
#' to the result
#'
#' @param deweathered
#' @param month
#'
#' @return
#' @export
#'
#' @examples
extract_yoy_changes <- function(deweathered, month) {
  
  new_results <- lapply(deweathered$result, function(result) {
    tryCatch(
      {
        extract_yoy_changes_from_result(result, month)
      },
      error = function(e) {
        return(NULL)
      }
    )
  })

  deweathered$result <- new_results
  return(deweathered)
}

#' Extract yoy change from a result df
#'
#' @param result df with date, variable, value columns
#' @param month
#'
#' @return df with date, variable, value columns
#' @export
#'
#' @examples
extract_yoy_changes_from_result <- function(result, month) {
  dates <- get_excluded_yoy_dates(month)

  result %>%
    filter(
      date %in% dates,
      variable %in% c("observed", "predicted")
    ) %>%
    mutate(year = year(date)) %>%
    group_by(variable, year) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    arrange(year) %>%
    group_by(across(-c(year, value))) %>%
    mutate(delta = value - lag(value)) %>%
    # Add observed value of first year
    ungroup() %>%
    mutate(observed_prev = value[year == min(year) & variable == "observed"]) %>%
    ungroup() %>%
    filter(!is.na(delta)) %>%
    select(-c(value)) %>%
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "delta",
      names_prefix = "yoy_"
    ) %>%
    rename(
      yoy_total = yoy_observed,
      yoy_weather = yoy_predicted
    ) %>%
    mutate(
      yoy_emission = yoy_total - yoy_weather,
      yoy_total_rel = yoy_total / observed_prev,
      yoy_weather_rel = yoy_weather / observed_prev,
      yoy_emission_rel = yoy_emission / observed_prev
    ) %>%
    pivot_longer(
      cols = -c(year, observed_prev),
      names_to = "variable",
      values_to = "value",
    ) %>%
    # add month to variable
    mutate(date = as.Date(month)) %>%
    select(date, variable, value)
}



#' For a given month, return the dates that need to be removed from training
#' i.e. dates in this month and the same month the year before
#'
#' @param month
#'
#' @return
#' @export
#'
#' @examples
get_excluded_yoy_dates <- function(month) {
  month_f <- as.Date(month)
  month_i <- month_f - lubridate::years(1)

  # Get all days in month_f
  days_f <- seq.Date(as.Date(month_f), as.Date(month_f) + lubridate::days_in_month(month_f) - 1, by = "day")
  days_i <- seq.Date(as.Date(month_i), as.Date(month_i) + lubridate::days_in_month(month_i) - 1, by = "day")

  c(days_i, days_f)
}
