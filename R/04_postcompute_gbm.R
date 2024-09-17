postcompute_gbm <- function(models, data, config, performances, ...) {
  weather_vars <- config$weather_vars
  time_vars <- config$time_vars
  add_fire <- config$add_fire
  formula_vars <- models[[1]]$var.names


  if (config$link == "linear") {
    do_link <- function(x) {
      x
    }
    do_unlink <- function(x) {
      x
    }
  } else if (config$link == "log") {
    do_link <- function(x) {
      log(x)
    }
    do_unlink <- function(x) {
      exp(x)
    }
  }

  # Predict results ---------------------------------------------------------
  # Each dataset can have multiple models if ntrainings > 1
  # Add an index to simplify
  data$index <- zoo::index(data)
  predicted <- lapply(models, function(model) {
      tibble(
        index = data$index,
        predicted = do_unlink(predict(model, data, n.trees = model$n.trees.opt))
      )
    }) %>%
    bind_rows() %>%
    group_by(index) %>%
    summarise(
      predicted = mean(predicted),
      predicted_p975 = quantile(predicted, 0.975),
      predicted_p025 = quantile(predicted, 0.025)
    )

  data <- data %>%
    select(-c(predicted)) %>% # first model was used to predict already, in train_gbm
    left_join(predicted, by = "index")

  # data$predicted <- do_unlink(gbm::predict.gbm(model, data))
  data$value <- do_unlink(data$value)
  data$observed <- data$value
  data$anomaly <- data$observed - data$predicted


  # If fire, we create a no_fire counterfactual
  if (add_fire) {
    data <- postcompute_gbm_fire(
      data = data, models = models, do_unlink = do_unlink,
      weather_vars = weather_vars
    )
  }

  # Extract influence of time vars (e.g. trend)
  if (length(time_vars) > 0) {
    data <- postcompute_gbm_trends(
      data = data, time_vars = time_vars, models = models,
      do_unlink = do_unlink
    )
  }

  # Keep only useful information
  cols <- c("date", "anomaly", "predicted", "observed", "trend")
  # Add cols starting with trend_ (though ignoring for now)
  cols <- c(cols, names(data)[grepl("trend_", names(data))])
  if (add_fire) {
    cols <- c(cols, names(data)[grepl("predicted_nofire", names(data))])
  }

  result <- data %>%
    ungroup() %>%
    dplyr::select_at(intersect(cols, names(data))) %>%
    mutate(date = date(date)) %>%
    arrange(date) %>%
    tidyr::gather("variable", "value", -c(date))

  # Build a lite version of the model for saving purposes
  models_light <- postcompute_gbm_lighten_model(models = models, data = data)

  return(
    tibble(
      config = list(config),
      models = list(models_light),
      result = list(result),
      performances = list(performances)
    )
  )
}


postcompute_gbm_lighten_model <- function(models, data) {
  #-------------------------------------
  # Add model details and performance
  #-------------------------------------

  # We only keep 'useful' information to save space
  # Can take several MB per model otherwise
  # model_light <- model[c("shrinkage",
  #                        "train.fraction",
  #                        "cv.folds")]

  # metrics <- lapply(split(data, data$set), function(d){
  #   tibble(
  #     set=unique(d$set),
  #     rmse=Metrics::rmse(d$value, d$predicted),
  #     rsquared=cor(d$value, d$predicted)^2
  #   )
  # }) %>%
  #   do.call(bind_rows, .) %>%
  #   tidyr::pivot_wider(names_from=set, values_from=c(rmse, rsquared)) %>%
  #   as.list()
  #
  # model_light <- model_light %>%
  #   modifyList(metrics)

  # Variable importance
  importance <- lapply(models, function(m) summary(m, plot_it = F))
  ntrees_opt <- lapply(models, function(m) m$n.trees.opt)
  # model_light$importance <- summary(model, plotit = F)

  return(
    list(
      importance = importance,
      ntrees_opt = ntrees_opt
    )
  )
}


postcompute_gbm_fire <- function(data, models, formula_vars, do_unlink, weather_vars) {
  
  formula_vars <- models[1]$var.names
  fire_vars <- formula_vars[grepl("fire|pm25_emission", formula_vars)]
  fire_vars <- fire_vars[!grepl("_lag[[:digit:]]*$", fire_vars)]

  for (fire_var in fire_vars) {
    data_nofire <- data
    data_nofire[, fire_var] <- 0
    # For lag
    data_nofire[, grepl(paste0(fire_var, "_lag[[:digit:]]*$"), names(data_nofire))] <- 0
    predicted_name <- paste0("predicted_nofire", gsub("fire_frp", "", fire_var))
    data[, predicted_name] <- sapply(models, function(model) do_unlink(predict(model, data_nofire, n.trees=model$n.trees.opt))) %>%
      rowMeans(na.rm = T)
  }

  # Do a general no fire
  fire_vars <- formula_vars[grepl("fire|pm25_emission", formula_vars)]
  data_nofire <- data
  data_nofire[, fire_vars] <- 0
  predicted_name <- "predicted_nofire"
  data[, predicted_name] <- sapply(models, function(model) do_unlink(predict(model, data_nofire, n.trees=model$n.trees.opt))) %>%
    rowMeans(na.rm = T)
  return(data)
}


postcompute_gbm_trends <- function(data, time_vars, models, do_unlink) {
  
  dates <- data %>%
    distinct(date) %>%
    arrange(date) %>%
    dplyr::mutate(yday_joiner = lubridate::yday(date)) %>%
    dplyr::mutate(month_joiner = lubridate::month(date)) %>%
    dplyr::mutate(wday_joiner = lubridate::wday(date, week_start = 1)) %>%
    dplyr::mutate(season_joiner = forcats::fct_collapse(
      .f = factor(lubridate::month(date)),
      Spring = c("3", "4", "5"),
      Summer = c("6", "7", "8"),
      Autumn = c("9", "10", "11"),
      Winter = c("12", "1", "2")
    ))


  if ("date_unix" %in% time_vars) {
    ndays <- as.numeric(max(dates$date) - min(dates$date), units = "days")
    
    trend_trend <- lapply(models, function(model) plot(model, "date_unix", 
                                                       continuous_resolution =ndays, return_grid = T)) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(y = do_unlink(y)) %>%
      ungroup() %>%
      mutate(date = lubridate::date(lubridate::date_decimal(date_unix))) %>%
      dplyr::select(date, date_unix = y) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(
        trend = mean(date_unix, na.rm = T),
        trend_p975 = quantile(date_unix, 0.975, na.rm = T),
        trend_p025 = quantile(date_unix, 0.025, na.rm = T)
      )

    dates <- dates %>% left_join(trend_trend)
  }

  # if("yday" %in% time_vars){
  #   trend_jday <- plot(model, "yday", continuous_resolution = 366, return_grid = T) %>%
  #     rowwise() %>%
  #     mutate(y=do_unlink(y)) %>%
  #     mutate(jday=round(yday)) %>%
  #     dplyr::group_by(yday) %>%
  #     dplyr::summarize(y=mean(y, na.rm=T)) %>%
  #     dplyr::select(yday_joiner=yday, trend_yday=y)

  #   dates <- dates %>% left_join(trend_jday)
  # }

  # if("month" %in% time_vars){
  #   trend_month <- plot(model, "month", return_grid = T) %>%
  #     rowwise() %>%
  #     mutate(y=do_unlink(y)) %>%
  #     dplyr::select(month_joiner=month, trend_month=y)
  #   trend_month$month_joiner <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,
  #                                 aug=8,sep=9,oct=10,nov=11,dec=12)[tolower(trend_month$month_joiner)]

  #   dates <- dates %>% left_join(trend_month)
  # }

  # if("weekday" %in% time_vars){
  #   trend_weekday <- plot(model, "weekday", return_grid = T) %>%
  #     rowwise() %>%
  #     mutate(y=do_unlink(y)) %>%
  #     dplyr::select(wday_joiner=weekday, trend_weekday=y)
  #   trend_weekday$wday_joiner <- c(monday=1,tuesday=2,wednesday=3,thursday=4,friday=5,
  #                                  saturday=6,sunday=7)[tolower(trend_weekday$wday_joiner)]

  #   dates <- dates %>% left_join(trend_weekday)
  # }

  # if("season" %in% time_vars){
  #   trend_season <- plot(model, "season", return_grid = T) %>%
  #     rowwise() %>%
  #     mutate(y=do_unlink(y)) %>%
  #     dplyr::select(season_joiner=season, trend_season=y)

  #   dates <- dates %>% left_join(trend_season)
  # }

  # Select date and variables starting with 'trend_'
  trend_vars <- dates %>% dplyr::select(date, starts_with("trend"))

  return(
    data %>%
      full_join(trend_vars)
  )
}
