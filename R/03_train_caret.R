#' Training a model using standard gmb and comparing observed vs predicted as anomaly
#'
#'
#' The data is divided in three sets.
#' A training period that is data fed to GBM
#' (e.g. three years before the period we want deweathered data from when using 
#' anomaly approach)
#' 
#' This training period is itself divided into:
#'  - training GBM per se (`training.fraction`)
#'  - validation by GBM (1-`training.fraction`)
#'
#' 
#' A prediction period which is the period for which we want to get deweathered data
#' (when using anomaly approach)
#'
#' @param location_id 
#' @param data 
#' @param pollutant 
#' @param unit 
#' @param training_end
#' @param link: either null or 'log'
#'
#' @return
#' @export
#'
#' @examples
train_caret <- function(data,
                        training_end,
                        weather_vars,
                        time_vars,
                        mehod="gbm",
                        normalise,
                        trees,
                        training.fraction=0.9,
                        interaction.depth=1,
                        learning.rate=0.1,
                        link="linear",
                        ...){
  
  
  if(is.null(training_end)){
    training_end <- "2099-01-01"
  }
  
  n_cores <- as.integer(future::availableCores()-1)
  
  if(!link %in% c('linear','log')){
    stop("link can only be 'linear' or 'log'")
  }
  
  if(link=="linear"){
    do_link <- function(x){x}
    do_unlink <- function(x){x}
  }else if(link=="log"){
    do_link <- function(x){log(x)}
    do_unlink <- function(x){exp(x)}
  }
  
  
  # Using deweather to prepare data re time vars
  # Correspondance between our time variables and deweather ones
  # our=deweather
  time_vars_corr <- list(
    "date_unix"="trend",
    "wday"="weekday",
    "month"="month",
    "week"="week",
    "yday"="jday",
    "hour"="hour"
  )
  
  if(any(!time_vars %in% c(names(time_vars_corr), colnames(data)))){
    stop(paste("Deweather can only create the following timevars:", paste(names(time_vars_corr), collapse=",")))
  }else{
    time_vars <- c(unlist(time_vars_corr[time_vars], use.names=F), setdiff(time_vars,names(time_vars_corr)))
  }
  
  if("geometry" %in% names(data)){
    data <- data %>% dplyr::select(-c(geometry)) 
  }
  
  
  # Prepare data ------------------------------------------------------------
  data_prepared <- data %>%
    mutate(date=as.POSIXct(date)) %>%
    deweather::prepData(add=time_vars)
  
  
  # Reshuffle as it seems GBM doesn't do it
  # Very important
  if(training.fraction<1){
    rows <- sample(nrow(data_prepared))
    data_prepared <- data_prepared[rows, ]
  }
  
  data_prepared[data_prepared$date > training_end,'set'] <- "prediction"
  data_prepared[data_prepared$date <= training_end,'set'] <- "training" # Actually, gbm will use a fraction of it for validation
  
  # Train model -------------------------------------------------------------
  
  model_gbm  <- function(training_data, formula){
    print("Training gbm")
    gbm.fit <- gbm3::gbm(
      formula = formula,
      data = training_data,
      distribution='gaussian',
      cv.folds = 3,
      shrinkage=learning.rate,
      interaction.depth=interaction.depth,
      train.fraction = training.fraction,
      n.cores = n_cores,
      n.trees = trees, # Actually the maximum number of trees. We'll use the optimal one indicated by CV
      verbose = FALSE,
      keep.data = F
    )
    print("Done")
    return(gbm.fit)
  }
  
  formula_vars <- c(time_vars, weather_vars)
  formula <- reformulate(termlabels=formula_vars,
                         response='value')  
  
  data_prepared <- data_prepared %>%
    dplyr::filter_at(formula_vars, any_vars(!is.na(.))) %>%
    dplyr::filter_at("value", all_vars(!is.na(.)))
  
  # Add "link" transformation if required
  data_prepared$value <- do_link(data_prepared$value)
  
  # Fit
  model <- model_gbm(data_prepared %>% dplyr::filter(set=="training" & !is.na(value) & !is.infinite(value)), formula) 
  
  # Optimal number of trees
  # Two options: OOB or CV
  # Apparently, CV is better on large datasets: https://towardsdatascience.com/understanding-gradient-boosting-machines-9be756fe76ab
  # Plus we won't have OOB tree number if train.fraction=1
  n.trees.opt <- gbm3::gbm.perf(model, method="cv", plot.it = F)
  # n.trees.opt <- gbm::gbm.perf(model, method="OOB")
  print(sprintf("Using %d trees (based on CV results)", n.trees.opt))
  

  # Predict results ---------------------------------------------------------
  data_prepared$predicted <- gbm::predict.gbm(model,
                                              data_prepared,
                                              n.trees=n.trees.opt)
  
  # If fire was part of weather variables
  # We create a no_fire counterfactual
  add_nofire <- any(stringr::str_detect(weather_vars, "fire|pm25_emission"))
  if(add_nofire){
    data_prepared_nofire <- data_prepared
    data_prepared_nofire[, grep("fire|pm25_emission", names(data_prepared))] <- 0
    data_prepared$predicted_nofire <- predict(model,
                                              data_prepared_nofire,
                                              n.trees=n.trees.opt)
  }
  
  data_prepared$value <- do_unlink(data_prepared$value)
  data_prepared$predicted <- do_unlink(data_prepared$predicted)
  
  if(add_nofire){
    data_prepared$predicted_nofire <- do_unlink(data_prepared$predicted_nofire)
  }
  
  data_prepared$residuals <- data_prepared$predicted - data_prepared$value
  
  
  # Add performance metrics ----------------------------------------------------
  
  # We only keep 'useful' information to save space
  # Can take several MB per model otherwise
  model_light <- model[c("shrinkage",
                         "train.fraction",
                         "cv.folds")]
  
  # model_light$cv.error <- model_light$cv.error[n.trees.opt]
  # model_light$train.error <- model_light$train.error[n.trees.opt]
  # model_light$valid.error <- model_light$valid.error[n.trees.opt]
  
  # Another way to get RMSE (see for instance http://uc-r.github.io/gbm_regression)
  model_light$rmse_crossvalid <- sqrt(do_unlink(model$cv.error[n.trees.opt]))
  model_light$rmse_train <- sqrt(do_unlink(model$train.error[n.trees.opt]))
  model_light$rmse_valid <- sqrt(do_unlink(model$valid.error[n.trees.opt]))
  
  # Metrics on predicting period
  # Not that if values highly deviated (e.g. because of COVID)
  # then it is normal to have large errors. This is precisely why we're using it
  data_predict <- data_prepared %>% filter(set=="prediction") %>% filter(!is.na(value) & !is.infinite(value))
  model_light$rmse_predict <- Metrics::rmse(data_predict$value, data_predict$predicted)
  model_light$mae_predict <- Metrics::mae(data_predict$value, data_predict$predicted)
  model_light$rsquared_predict <- 1 - sum((data_predict$predicted - data_predict$value)^2) / sum((data_predict$value - mean(data_predict$value))^2)
  
  # Metrics on whole training
  data_training <- data_prepared %>% filter(set=="training") %>% filter(!is.na(value) & !is.infinite(value))
  model_light$rmse_trainval<- Metrics::rmse(data_training$value, data_training$predicted)
  model_light$mae_trainval <- Metrics::mae(data_training$value, data_training$predicted)
  model_light$rsquared_trainval <- 1 - sum((data_training$predicted - data_training$value)^2) / sum((data_training$value - mean(data_training$value))^2)
  
  # Variable importance
  model_light$importance <- summary(model, plotit = F)
  
  cols <- c("date", "set", "value", "predicted")
  if(add_nofire){
    cols <- c(cols, "predicted_nofire")
  }
  
  res <- tibble(model=list(model_light),
                predicted=list(data_prepared %>% dplyr::select_at(cols) %>% arrange(date))
  )
  
  if(normalise){
    warning("Normalised not managed yet for gbm engine. Use deweather instead if normalisation is your goal")
  }
  
  # Extract influence of time vars (e.g. trend) -----------------------------
  
  if(length(time_vars)>0){
    
    dates <- tibble(date=lubridate::date(unique(data_prepared %>% filter(set=='training') %>% pull(date)))) %>%
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
      # TODO: Check it gives similar results
      trend_trend <- gbm::plot.gbm(model, "trend", continuous.resolution = nrow(dates)*2, return.grid = T) %>%
        rowwise() %>%
        mutate(y=do_unlink(y)) %>%
        mutate(date=lubridate::date(lubridate::date_decimal(trend))) %>%
        dplyr::select(date,trend=y) %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(trend=mean(trend, na.rm=T))
      
      dates <- dates %>% left_join(trend_trend)
    }
    
    if("jday" %in% time_vars){
      trend_jday <- gbm::plot.gbm(model, "jday",continuous.resolution = 366, return.grid = T) %>%
        rowwise() %>%
        mutate(y=do_unlink(y)) %>%
        mutate(jday=round(jday)) %>% 
        dplyr::group_by(jday) %>%
        dplyr::summarize(y=mean(y, na.rm=T)) %>%
        dplyr::select(yday_joiner=jday, jday=y) 
      
      dates <- dates %>% left_join(trend_jday)
    }
    
    if("month" %in% time_vars){
      trend_month <- gbm::plot.gbm(model, "month", return.grid = T) %>%
        rowwise() %>%
        mutate(y=do_unlink(y)) %>%
        dplyr::select(month_joiner=month, month=y)
      trend_month$month_joiner <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,
                                    aug=8,sep=9,oct=10,nov=11,dec=12)[tolower(trend_month$month_joiner)]
      
      dates <- dates %>% left_join(trend_month)
    }
    
    if("weekday" %in% time_vars){
      trend_weekday <- gbm::plot.gbm(model, "weekday", return.grid = T) %>%
        rowwise() %>%
        mutate(y=do_unlink(y)) %>%
        dplyr::select(wday_joiner=weekday, weekday=y)
      trend_weekday$wday_joiner <- c(monday=1,tuesday=2,wednesday=3,thursday=4,friday=5,
                                     saturday=6,sunday=7)[tolower(trend_weekday$wday_joiner)]
      
      dates <- dates %>% left_join(trend_weekday)
    }
    
    if("season" %in% time_vars){
      trend_season <- gbm::plot.gbm(model, "season", return.grid = T) %>%
        rowwise() %>%
        mutate(y=do_unlink(y)) %>%
        dplyr::select(season_joiner=season, season=y)
      
      dates <- dates %>% left_join(trend_season)
    }
    
    trend <- tibble(
      date=dates$date,
      value= dates %>% dplyr::select(time_vars) %>% rowMeans(na.rm=TRUE))
    
    res$trend <- list(tibble(trend))
  }
  
  res
}
