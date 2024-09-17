create_configs <- function(
  weather_vars,
  time_vars,
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
  ntrainings=1,
  ...){
  

  configs <- list(
    list(
      output = 'anomaly',
      time_vars = list(time_vars),
      training_end = training_end_anomaly,
      training_start = training_start_anomaly
    ),
    list(
      output = 'trend',
      time_vars = list(time_vars),
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
                  training_excluded_dates=list(training_excluded_dates),
                  ntrainings
                  )
  
  return(configs)
}



