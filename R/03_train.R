#' Training models with several potential engines: deweather, rmweather or gbm directly
#' (SVR not yet implemented)
#'
#' @param meas_weather 
#' @param pollutants 
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
train_models <- function(meas_weather,
                         engine,
                         pollutants=NULL,
                         trees=600,
                         samples=300,
                         lag=0,
                         weather_vars=NULL,
                         time_vars=NULL,
                         training.fraction=1,
                         normalise=F,
                         detect_breaks=F,
                         training_date_cut,
                         ...
){
  
  if(nrow(meas_weather)==0){
    warning("No measurements available. Returning NA")
    return(NA)
  }
  
  # Check input
  if(!engine %in% c('gbm', 'svr', 'rmweather', 'deweather')){
    stop("'engine' should be either 'gbm', 'svr', 'rmweather' or 'deweather'")
  }
  
  
  # Filter input data
  # Only keep those with values in 2020 and some wind data
  meas_weather <- meas_weather %>% rowwise() %>% filter(max(meas_weather$date, na.rm=T)>'2020-01-01')
  if(nrow(meas_weather)==0){
    warning("No measurements available in 2020. Returning NA")
    return(NA)
  }
  meas_weather <- meas_weather %>% filter(!all(is.na(meas_weather$ws)) & length(unique(meas_weather$ws))>1)
  
  # Filter pollutants
  if(!is.null(pollutants)){
    meas_weather <- meas_weather %>% filter(pollutant %in% pollutants)
  }
  
  # Select variables
  weather_vars_available <- setdiff(colnames(meas_weather$meas_weather[[1]]), c('date','value'))
  if(is.null(weather_vars)){
    weather_vars <- c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH_max', 'pbl_max', 'sunshine')  
  }
  
  # Only keep rows with weather vars
  meas_weather <- meas_weather %>% rowwise() %>%
    mutate(meas_weather=list(meas_weather %>%
                               dplyr::filter_at(vars(weather_vars), any_vars(!is.na(.)))))
  
  # Add lag if need be
  if(!is.null(lag) & lag>0){
    day_lags <- c(1:lag)  
    weather_vars_lags <- c(weather_vars, unlist(lapply(weather_vars, function(x) paste(x,day_lags,sep="_"))))
    meas_weather_lag <- meas_weather %>% rowwise() %>%
      mutate(meas_weather=list(utils.add_lag(meas_weather, weather_vars, group_cols=c(), day_lags, 'day')))
  }else{
    weather_vars_lags <- weather_vars
    meas_weather_lag <- meas_weather %>% rowwise()
  }
  
  # Add certain time vars if required (rmweather doesn't handle month, none handles season etc)
  if("month" %in% time_vars){
    meas_weather_lag <- meas_weather_lag %>%
      mutate(meas_weather=list(meas_weather %>% mutate(month=lubridate::month(date))))
  }
  
  if("season" %in% time_vars){
    meas_weather_lag <- meas_weather_lag %>%
      mutate(meas_weather=list(meas_weather %>% mutate(
            season = forcats::fct_collapse(
              .f = factor(lubridate::month(date)),
              Spring = c("3","4","5"),
              Summer = c("6","7","8"),
              Autumn = c("9","10","11"),
              Winter = c("12","1","2")
            )
          )
        )
      )
  }
  
  
  # Train models
  train_model <- switch(engine,
         "svr"=train_model_svr,               
         "gbm"=train_model_gbm,
         "rmweather"=train_model_rmweather,
         "deweather"=train_model_deweather
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
  
  meas_weather_lag$index <- zoo::index(meas_weather_lag)
  # n_cores <- as.integer(future::availableCores()-1)
  
  result <- pbapply::pbmapply(train_model_safe,
                     index=meas_weather_lag$index,
                     location_id=meas_weather_lag$location_id,
                     data=meas_weather_lag$meas_weather,
                     normalise=normalise,
                     training_date_cut=training_date_cut,
                     weather_vars=list(weather_vars_lags),
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
  result <- result %>% dplyr::left_join(meas_weather_lag %>% dplyr::select(-c(meas_weather)),
                                        by="index") %>%
    dplyr::select(-c(index))
  
  return(result)
}








