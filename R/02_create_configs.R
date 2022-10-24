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
  trees=10000,
  samples=300,
  interaction.depth=c(2),
  learning.rate=c(0.01),
  normalise=F,
  detect_breaks=F,
  keep_model=T,
  ...){
  
  
  time_vars_output <- tibble(
    time_vars=c(list(c('yday')), list(c()), list(c('date_unix'))),
    output=c('anomaly_yday', 'anomaly', 'trend'),
    training_end=c(training_end_anomaly, training_end_anomaly, training_end_trend),
    training_start=c(training_start_anomaly, training_start_anomaly, training_start_trend)
  ) %>%
    filter(output %in% !!output)
  
  configs <-  tibble() %>%
    tidyr::expand(trees,
                  samples,
                  lag,
                  training.fraction,
                  weather_vars=list(unique(weather_vars)),
                  add_fire,
                  time_vars_output,
                  engine,
                  link,
                  learning.rate,
                  interaction.depth,
                  cv_folds,
                  keep_model)
  
  return(configs)
}



