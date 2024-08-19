postcompute_gbm <- function(model, data, config, ...){
  
  weather_vars <- config$weather_vars
  time_vars <- config$time_vars
  formula_vars <- model$var.names
  add_fire <- config$add_fire
  
  if(config$link=="linear"){
    do_link <- function(x){x}
    do_unlink <- function(x){x}
  }else if(config$link=="log"){
    do_link <- function(x){log(x)}
    do_unlink <- function(x){exp(x)}
  }
  
  # Predict results ---------------------------------------------------------
  data$predicted <- do_unlink(gbm::predict.gbm(model, data))
  data$value <- do_unlink(data$value)
  data$observed <- data$value
  data$anomaly <- data$observed - data$predicted
  
  # Build a lite version of the model for saving purposes
  model_light <- postcompute_gbm_lighten_model(model=model, data=data)
  
  # If fire, we create a no_fire counterfactual
  if(add_fire){
    data <- postcompute_gbm_fire(data=data, model=model, do_unlink=do_unlink,
                                 weather_vars=weather_vars)
  }
  
  # Extract influence of time vars (e.g. trend)
  if(length(time_vars)>0){
    data <- postcompute_gbm_trends(data=data, time_vars=time_vars, model=model,
                                  do_unlink=do_unlink)
  }
  
  # Keep only useful information
  cols <- c('date', 'anomaly', 'predicted', 'observed', 'trend')
  # Add cols starting with trend_ (though ignoring for now)
  # cols <- c(cols, names(data)[grepl("trend_", names(data))])
  if(add_fire){
    cols <- c(cols, names(data)[grepl("predicted_nofire", names(data))])
  }
  
  result <- data %>%
    ungroup() %>%
    dplyr::select_at(intersect(cols, names(data))) %>%
    mutate(date=date(date)) %>%
    arrange(date) %>%
    tidyr::gather('variable', 'value', -c(date))

  
  return(
    tibble(
      config=list(config),
      model=list(model_light),
      result=list(result)
    )
  )
}


postcompute_gbm_lighten_model <- function(model, data){
  
  #-------------------------------------
  # Add model details and performance
  #-------------------------------------
  
  # We only keep 'useful' information to save space
  # Can take several MB per model otherwise
  model_light <- model[c("shrinkage",
                         "train.fraction",
                         "cv.folds")]
  
  metrics <- lapply(split(data, data$set), function(d){
    tibble(
      set=unique(d$set),
      rmse=Metrics::rmse(d$value, d$predicted),
      rsquared=cor(d$value, d$predicted)^2
    )
  }) %>%
    do.call(bind_rows, .) %>%
    tidyr::pivot_wider(names_from=set, values_from=c(rmse, rsquared)) %>%
    as.list()
  
  model_light <- model_light %>%
    modifyList(metrics)
  
  # Variable importance
  model_light$importance <- summary(model, plotit = F)
  
  return(model_light)
}

postcompute_gbm_fire <- function(data, model, formula_vars, do_unlink, weather_vars){
  
  formula_vars <- model$var.names
  fire_vars <- formula_vars[grepl("fire|pm25_emission", formula_vars)]
  fire_vars <- fire_vars[!grepl("_lag[[:digit:]]*$", fire_vars)]
  
  for(fire_var in fire_vars){
    data_nofire <- data
    data_nofire[, fire_var] <- 0
    # For lag
    data_nofire[, grepl(paste0(fire_var,"_lag[[:digit:]]*$"), names(data_nofire))] <- 0
    predicted_name <- paste0("predicted_nofire", gsub("fire_frp","",fire_var))
    data[, predicted_name] <- do_unlink(predict(model,
                                                data_nofire))
  }
  
  # Do a general no fire
  fire_vars <- formula_vars[grepl("fire|pm25_emission", formula_vars)]
  data_nofire <- data
  data_nofire[, fire_vars] <- 0
  predicted_name <-"predicted_nofire"
  data[, predicted_name] <- do_unlink(predict(model, data_nofire))
  return(data)
}


postcompute_gbm_trends <- function(data, time_vars, model, do_unlink){

    dates <- tibble(date=lubridate::date(unique(data %>% filter(set=='training') %>% pull(date)))) %>%
      dplyr::mutate(yday_joiner=lubridate::yday(date)) %>%
      dplyr::mutate(month_joiner=lubridate::month(date)) %>%
      dplyr::mutate(wday_joiner=lubridate::wday(date, week_start=1)) %>%
      dplyr::mutate(season_joiner = forcats::fct_collapse(
        .f = factor(lubridate::month(date)),
        Spring = c("3","4","5"),
        Summer = c("6","7","8"),
        Autumn = c("9","10","11"),
        Winter = c("12","1","2")
      ))
  
  if("date_unix" %in% time_vars){
    # Rather than resimulating with average weather conditions,
    # we take partial dependency as the deweathered trend
    # TODO: Check it gives similar 
    trend_trend <- gbm::plot.gbm(model, "date_unix", continuous.resolution = nrow(dates)*2, return.grid = T) %>%
      rowwise() %>%
      mutate(y=do_unlink(y)) %>%
      mutate(date=lubridate::date(lubridate::date_decimal(date_unix))) %>%
      dplyr::select(date, date_unix=y) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(trend=mean(date_unix, na.rm=T))
    
    dates <- dates %>% left_join(trend_trend)
  }
  
  if("yday" %in% time_vars){
    trend_jday <- gbm::plot.gbm(model, "yday",continuous.resolution = 366, return.grid = T) %>%
      rowwise() %>%
      mutate(y=do_unlink(y)) %>%
      mutate(jday=round(yday)) %>% 
      dplyr::group_by(yday) %>%
      dplyr::summarize(y=mean(y, na.rm=T)) %>%
      dplyr::select(yday_joiner=yday, trend_yday=y) 
    
    dates <- dates %>% left_join(trend_jday)
  }
  
  if("month" %in% time_vars){
    trend_month <- gbm::plot.gbm(model, "month", return.grid = T) %>%
      rowwise() %>%
      mutate(y=do_unlink(y)) %>%
      dplyr::select(month_joiner=month, trend_month=y)
    trend_month$month_joiner <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,
                                  aug=8,sep=9,oct=10,nov=11,dec=12)[tolower(trend_month$month_joiner)]
    
    dates <- dates %>% left_join(trend_month)
  }
  
  if("weekday" %in% time_vars){
    trend_weekday <- gbm::plot.gbm(model, "weekday", return.grid = T) %>%
      rowwise() %>%
      mutate(y=do_unlink(y)) %>%
      dplyr::select(wday_joiner=weekday, trend_weekday=y)
    trend_weekday$wday_joiner <- c(monday=1,tuesday=2,wednesday=3,thursday=4,friday=5,
                                   saturday=6,sunday=7)[tolower(trend_weekday$wday_joiner)]
    
    dates <- dates %>% left_join(trend_weekday)
  }
  
  if("season" %in% time_vars){
    trend_season <- gbm::plot.gbm(model, "season", return.grid = T) %>%
      rowwise() %>%
      mutate(y=do_unlink(y)) %>%
      dplyr::select(season_joiner=season, trend_season=y)
    
    dates <- dates %>% left_join(trend_season)
  }
    
  # Select date and variables starting with 'trend_'
  trend_vars <- dates %>% dplyr::select(date, starts_with("trend"))
  
  return(
    data %>%
      full_join(trend_vars))
}