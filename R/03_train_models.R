#' Title
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
#' @param exp_suffix 
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
                         normalise=F,
                         detect_breaks=F,
                         training_date_cut,
                         exp_name=NULL,
                         exp_suffix=NULL,
                         return_result=T, # If False, return folder if exists
                         save_result=T
){
  
  # Check input
  if(!engine %in% c('gbm', 'rmweather', 'deweather')){
    stop("'engine' should be either 'gbm', 'rmweather' or 'deweather'")
  }
  
  # Experiment name for storage
  if(is.null(exp_name)){
    exp_name <- paste0('lag',lag,'_',paste(tolower(pollutants),collapse='_'),
                       '_trees',trees,
                       '_samples',samples,
                       '_norm',substr(as.character(normalise),1,1))
    if(!is.null(exp_suffix)){
      exp_name <- paste0(exp_name, exp_suffix)  
    }
  }
  
  # Files and folder
  if(save_result){
    output_folder <- file.path('data', '03_train_models', 'output')
    if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
    org_file <- ifelse(rstudioapi::isAvailable(),
                       rstudioapi::getActiveDocumentContext()$path,
                       'R/03_train_model_rmweather.R')
  }
  
  
  # Filter input data
  # Only keep those with values in 2020 and some wind data
  meas_weather <- meas_weather %>% rowwise() %>% filter(max(meas_weather$date)>'2020-01-01')
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
  
  # Add lag if need be
  if(!is.null(lag) & lag>0){
    day_lags <- c(1:lag)  
    weather_vars_lags <- unlist(lapply(weather_vars, function(x) paste(x,day_lags,sep="_")))
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
         "gbm"=train_model_gbm,
         "rmweather"=train_model_rmweather,
         "deweather"=train_model_deweather
  )

  train_model_safe <- function(index, ...){
    tryCatch({
      res <- train_model(...)
      res$index <- index
      return(res)
    }, error=function(err){
      warning(paste("Failed to train model:",err))
      NA
    })
  }
  
  meas_weather_lag$index <- index(meas_weather_lag)
    
  result <- pbmapply(train_model_safe,
                       index=meas_weather_lag$index,
                       data=meas_weather_lag$meas_weather,
                       normalise=normalise,
                       training_date_cut=training_date_cut,
                       weather_vars=list(weather_vars_lags),
                       time_vars=list(time_vars),
                       trees=trees,
                       detect_breaks=detect_breaks,
                       samples=samples,
                       mc.cores=1,
                       USE.NAMES=F,
                       SIMPLIFY=FALSE)
  
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
  
  
  ### Result folder
  if(save_result){
    # Create results folder
    timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
    result_folder <- file.path(output_folder, paste(timestamp_str,exp_name,sep='_'))
    dir.create(result_folder)
    
    # Save script file
    file.copy(org_file, result_folder, overwrite = T)
    file.rename(from = file.path(result_folder, basename(org_file)),
                to = file.path(result_folder, paste0(timestamp_str, '_', tools::file_path_sans_ext(basename(org_file)),'.R')))
    
    # Save result
    saveRDS(result, file=file.path(result_folder,paste0('result.RDS')))
  }
  
  if(return_result){
    return(result)
  }else{
    return(result_folder)  
  }
}








