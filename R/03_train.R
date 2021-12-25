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
                         trees=600,
                         samples=300,
                         lag=0,
                         weather_vars=NULL,
                         time_vars=NULL,
                         training.fraction=0.9,
                         normalise=F,
                         detect_breaks=F,
                         training_date_cut,
                         ...
){
  
  
  if(nrow(data)==0 || nrow(data$meas_weather[[1]])==0 || is.na(data$meas_weather[[1]])){
    warning("No measurements available. Returning NA")
    return(NA)
  }
  
  # Check input
  if(!engine %in% c('gbm', 'svr', 'rmweather', 'deweather', 'caret')){
    stop("'engine' should be either 'gbm', 'svr', 'rmweather', 'deweather', or 'caret")
  }
  
  if(!is.null(lag) & lag>0){
    day_lags <- c(1:lag)
    weather_vars_wlag <- c(weather_vars, unlist(lapply(weather_vars, function(x) paste(x,day_lags,sep="_"))))
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
         "svr"=train_svr,               
         "gbm"=train_gbm,
         "caret"=train_caret,
         "rmweather"=train_rmweather,
         "deweather"=train_deweather
  )

  train_model_safe <- function(index, location_id, ...){
    tryCatch({
      print(paste("Training model on location", location_id))
      res <- train_model(...)
      res$index <- index
      return(res)
    }, error=function(err){
      #warming(paste("Failed to train model:",err))
      print(paste("Failed to train model:",err))
      NA
    })
  }
  
  data$index <- zoo::index(data)
  # n_cores <- as.integer(future::availableCores()-1)
  
  result <- pbapply::pbmapply(train_model_safe,
                     index=data$index,
                     location_id=data$location_id,
                     data=data$meas_weather,
                     normalise=normalise,
                     training_date_cut=training_date_cut,
                     weather_vars=list(weather_vars_wlag),
                     time_vars=list(time_vars),
                     trees=trees,
                     detect_breaks=detect_breaks,
                     samples=samples,
                     training.fraction=training.fraction,
                     # mc.cores=n_cores,
                     USE.NAMES=F,
                     SIMPLIFY=FALSE,
                     ...)
  
  if(!is.null(result$value)){
    # When warning raised, sometimes actual results are in value column
    result <- do.call('bind_rows', result$value[!is.na(result$value)])
  }else{
    result <- do.call('bind_rows', result[!is.na(result)])
  }
  
  if(nrow(result)==0) return(NA)
  
  # Re-add indos
  result <- result %>% dplyr::left_join(data %>% dplyr::select(-c(meas_weather)),
                                        by="index") %>%
    dplyr::select(-c(index))
  
  return(result)
}








