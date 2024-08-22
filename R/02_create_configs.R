create_configs <- function(
  weather_vars,
  add_fire,
  output,
  engine,
  link,
  lag,
  cv_folds,
  training_start_anomaly,
  training_end_anomaly,
  training_start_trend,
  training_end_trend,
  training.fraction,
  training_excluded_dates=c(),
  trees=10000,
  samples=300,
  interaction.depth=c(2),
  learning.rate=c(0.01),
  normalise=F,
  detect_breaks=F,
  keep_model=T,
  ...){
  

  configs <- list(
    list(
      output = 'anomaly_yday',
      time_vars = list('yday'),
      training_end = training_end_anomaly,
      training_start = training_start_anomaly
    ),
    list(
      output = 'anomaly',
      time_vars = list(NULL),
      training_end = training_end_anomaly,
      training_start = training_start_anomaly
    ),
    list(
      output = 'trend',
      time_vars = list('date_unix'),
      training_end = training_end_trend,
      training_start = training_start_trend
    ),
    list(
      output = 'trend_yday',
      time_vars = list(c('date_unix', 'yday')),
      training_end = training_end_trend,
      training_start = training_start_trend
    )
  )
  
  # Convert list of lists into a tibble using map_dfr
  configs <- purrr::map_dfr(configs, as_tibble) %>%
    filter(output %in% !!output)
  
  # Add other parameters
  configs <-  tibble(configs) %>%
    tidyr::crossing(trees,
                  samples,
                  lag,
                  training.fraction,
                  weather_vars=list(unique(weather_vars)),
                  add_fire,
                  engine,
                  link,
                  learning.rate,
                  interaction.depth,
                  cv_folds,
                  keep_model,
                  training_excluded_dates=list(training_excluded_dates)
                  )
  
  return(configs)
}



