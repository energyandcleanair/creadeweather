
#' Title
#'
#' @param data 
#' @param configs 
#'
#' @return
#' @export
#'
#' @examples
train_configs <- function(data, configs){
  
  config_list <- lapply(split(configs, seq(nrow(configs))),
                        function(config){lapply(as.list(config), unlist)})
  
  lapply(config_list, function(x){
    result <- do.call(train_models,c(data=list(data), x))
    if(is.na(result)){
      return(NA)
    }
    result %>%
      mutate(config=list(x))
  }) %>%
  do.call(bind_rows, .)
} 



#' Training models with several potential engines: deweather, rmweather or gbm directly
#' (SVR not yet implemented)
#'
#' @param data 
#' @param deg 
#' @param trees 
#' @param samples 
#' @param lag 
#' @param normalise 
#' @param detect_breaks 
#' @param add_timestamp_var 
#'
#' @return
#' @export
#'
#' @examples
train_models <- function(data,
                         engine,
                         trees=1000,
                         samples=300,
                         lag=0,
                         weather_vars=NULL,
                         time_vars=NULL,
                         training.fraction=0.9,
                         normalise=F,
                         detect_breaks=F,
                         training_excluded_dates=NULL,
                         training_end,
                         ntrainings=1,
                         original_seed=42,
                         ...
){
  
  if(nrow(data)==0 || nrow(data$meas_weather[[1]])==0 || all(is.na(data$meas_weather[[1]]))){
    warning("No measurements available. Returning NA")
    return(NA)
  }
  
  # Check input
  if(!engine %in% c('gbm', 'svr', 'rmweather', 'deweather', 'caret')){
    stop("'engine' should be either 'gbm', 'svr', 'rmweather', 'deweather', or 'caret")
  }
  
  if(!is.null(lag) & lag>0){
    day_lags <- c(1:lag)
    weather_vars_wlag <- c(weather_vars, unlist(lapply(weather_vars, function(x) paste(x,day_lags,sep="_lag"))))
  }else{
    weather_vars_wlag <- weather_vars
  }
  
  
  # Add certain time vars if required (rmweather doesn't handle month, none handles season etc)
  if("month" %in% time_vars){
    data <- data %>%
      rowwise() %>%
      mutate(meas_weather=list(meas_weather %>% mutate(month=lubridate::month(date))))
  }
  
  if("season" %in% time_vars){
    data <- data %>%
      rowwise() %>%
      mutate(meas_weather=list(meas_weather %>%
                                 mutate(season = forcats::fct_collapse(
                                  .f = factor(lubridate::month(date)),
                                  Spring = c("3","4","5"),
                                  Summer = c("6","7","8"),
                                  Autumn = c("9","10","11"),
                                  Winter = c("12","1","2")))))
  }
  
  
  
  # Train models
  train_model <- switch(engine,
                        "gbm"=train_gbm
                        # "svr"=train_svr,               
                        # "caret"=train_caret,
                        # "rmweather"=train_rmweather,
                        # "deweather"=train_deweather
  )

  train_model_safe <- function(index, location_id, ...){
    tryCatch({
      print(paste("Training model on location", location_id))
      res <- train_model(...)
      res$index <- index
      return(res)
    }, error=function(err){
      print(paste("Failed to train model:",err))
      return(NA)
    })
  }
  

  train_model_safe_ntimes <- function(index, location_id, ...){
    set.seed(original_seed)
    lapply(
      1:ntrainings,
      function(i) {
        res <- train_model_safe(index, location_id, ...)
        set.seed(runif(1,1,1e6))
        return(res)}
    )
  }

  data$index <- zoo::index(data)

  result <- pbapply::pbmapply(
                     train_model_safe_ntimes,
                     index=data$index,
                     location_id=data$location_id,
                     data=data$meas_weather,
                     normalise=normalise,
                     training_end=training_end,
                     weather_vars=list(weather_vars_wlag),
                     time_vars=list(time_vars),
                     trees=trees,
                     detect_breaks=detect_breaks,
                     samples=samples,
                     training.fraction=training.fraction,
                     training_excluded_dates=list(training_excluded_dates),
                     USE.NAMES=F,
                     SIMPLIFY=FALSE,
                     ...)
  
  result <- result[!sapply(result, is.na)]
  
  if(length(result)==0){
    return(NA)
  }
  
  result <- bind_rows(unlist(result, recursive = FALSE) %>% na.omit)
    
  
  if(nrow(result)==0){
    return(NA)
  }
  
  result <- result %>%
    # Combine models, but lose additional information
    # that may have been added to data
    # (e.g. whether the variable was in training or testing set)
    # It would be too memory-demanding to keep all datasets if we do a 
    # bootstrap with say 100 models
    group_by(index) %>%
    summarise(models=list(model),
              performances=list(performance),
              data=list(first(data))
              ) %>%
    ungroup() %>%
    # Re-add infos
    dplyr::left_join(data %>% dplyr::select(-c(meas_weather)), by="index") %>%
    dplyr::select(-c(index))
  
  return(result)
}








