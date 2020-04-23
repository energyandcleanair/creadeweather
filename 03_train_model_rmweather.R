require(devtools)
require(rmweather)
require(dplyr)
require(purrr)
require(sf)
require(tibble)
require(lubridate)
require(ggplot2)
require(pbapply)
require(pbmcapply)
require(parallel)
require(future)
require(purrrlyr)

source('99_utils.R')
source('99_plot.R')

train_models_rmweather <- function(meas_weather,
                                   pollutants,
                                   deg,
                                   trees,
                                   samples,
                                   lag,
                                   normalise,
                                   add_timestamp_var=T,
                                   exp_suffix=NULL){
  
  output_folder <- file.path('data', '03_train_models', 'output')
  if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
  
  # Check we can find this file (to be copied later on)
  org_file <- if(rstudioapi::isAvailable()) rstudioapi::getActiveDocumentContext()$path else '03_train_model_rmweather.R'
  # meas_weather <- readRDS('data/02_prep_training/output/meas_w_weather_no2_02.RDS')
  
  exp_name <- paste0('lag',lag,'_',paste(tolower(pollutants),collapse='_'),'_deg',sub('\\.','',deg),
                     '_trees',trees,'_samples',samples,'_ts',substr(as.character(add_timestamp_var),1,1),'_norm',substr(as.character(normalise),1,1))
  if(!is.null(exp_suffix)){
    exp_name <- paste0(exp_name, exp_suffix)  
  }
  # exp_name <- paste('lag3',paste(pollutants, collapse='_'), no2_02deg_rmweather_trees600_samples600'
  n_trees <- trees
  samples <- samples

  
  stations_idx = NULL #seq(201,400) #NULL #seq(201,nrow(meas_weather)) #50 #You might not want to run every region / poll combination
  
  # only keep those with values in 2020 and some wind data
  meas_weather <- meas_weather %>% rowwise() %>% filter(max(meas_weather$date)>'2020-01-01')
  meas_weather <- meas_weather %>% filter(!all(is.na(meas_weather$ws)) & length(unique(meas_weather$ws))>1)
  
  
  if(!is.null(pollutants)){
    meas_weather <- meas_weather %>% filter(pollutant %in% pollutants)
  }
  
  if(!is.null(stations_idx)){
    stations <- unique(meas_weather$station_id)[stations_idx]
    meas_weather <- meas_weather %>% filter(station_id %in% stations)
  }
  
  weather_vars_available <- setdiff(colnames(meas_weather$meas_weather[[1]]), c('date','value'))
  weather_vars <- c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH', 'pbl_min', 'pbl_max', 'sunshine')
  
  if(lag>0){
    day_lags <- c(1:lag)  
    weather_vars_lags <- unlist(lapply(weather_vars, function(x) paste(x,day_lags,sep="_")))
    meas_weather_lag <- meas_weather %>% rowwise() %>%
      mutate(meas_weather=list(utils.add_lag(meas_weather, weather_vars, group_cols=c(), day_lags, 'day')))
  }else{
    weather_vars_lags <- weather_vars
    meas_weather_lag <- meas_weather %>% rowwise()
  }
  
  
  # formula <- reformulate(termlabels=weather_vars_lags,
  #                        response='value')
  # formula_vars <- vars(all.vars(formula))
  
  # Adding lag
  
  
  
  ### Result folder
  # Create results folder
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  result_folder <- file.path(output_folder, paste(timestamp_str,exp_name,sep='_'))
  dir.create(result_folder)
  
  # Save script file
  file.copy(org_file, result_folder, overwrite = T)
  file.rename(from = file.path(result_folder, basename(org_file)),
              to = file.path(result_folder, paste0(timestamp_str, '_', tools::file_path_sans_ext(basename(org_file)),'.R')))
  
  # Train models
  train_row <- function(station_id, data, pollutant, unit){
    
    tryCatch({
      data_prepared <- data %>%
        mutate(date=as.POSIXct(date)) %>%
        rmw_prepare_data(na.rm = TRUE)
      
      data_prepared_b <- data_prepared %>% filter(date<'2020-01-01')
      data_prepared_a <- data_prepared %>% filter(date>='2020-01-01')
      
      variables <- c("day_julian", "weekday",weather_vars_lags)
      if(add_timestamp_var){
        variables <- c(variables,"date_unix")
      }
      
      model <- rmw_train_model(
        df=data_prepared_b,
        variables = variables,
        n_trees = 1200,
        # n_samples = samples,
        verbose = F,
        keep_inbag = F, # Trying
        n_cores = 1
      )
      
      data_prepared$predicted <-rmw_predict(model, data_prepared)
      
      # save space
      model_light <- model
      model_light$forest<- NULL
      model_light$observations <- NULL
      
      res <- tibble(station_id=station_id, pollutant=pollutant, unit=unit) %>%
        mutate(model=list(model_light), predicted=list(data_prepared))
      
      if(normalise){
        normalised <-rmw_normalise(model, data_prepared, n_samples = samples)
        res <- res%>% mutate(normalised=list(normalised))
      } 
      
      res
    }, error=function(err){
      warning(paste("Station id failed:",station_id,':',err))
      return(NA)})
  }
  
  
  # We apply multicore training to chunks to avoid memory issues
  # Not optimal computation time wise but still decent for the safety / simplicity it brings
  nworkers <- as.integer(future::availableCores()-1)
  result <- pbmcmapply(train_row,
                              station_id=meas_weather_lag$station_id,
                              data=meas_weather_lag$meas_weather,
                              pollutant=meas_weather_lag$pollutant,
                              unit=meas_weather_lag$unit,
                              mc.cores=nworkers,
                       USE.NAMES=F,
                       SIMPLIFY=FALSE)
  
  result <- do.call('bind_rows',result[!is.na(result)])
  
  # result <- meas_weather_lag
  # result$model_fitted <- models_fitted
  saveRDS(result, file=file.path(result_folder,paste0('result.RDS')))
  
  # 
  # chunk_size <- 200
  # n_chunk <- nrow(meas_weather_lag) %/% chunk_size
  # i <- 0
  # for(chunk in split(meas_weather_lag, (seq(nrow(meas_weather_lag)) %/% chunk_size))){
  #   models_fitted <- pbmcmapply(train_row,
  #                               station_id=meas_weather_lag$station_id,
  #                               data=meas_weather_lag$meas_weather,
  #                               mc.cores=nworkers,
  #                               SIMPLIFY=FALSE)
  #   
  #   
  #   result <- chunk %>% select(-c(meas_weather))
  #   result$model_fitted <- models_fitted
  #   saveRDS(result, file=file.path(result_folder,paste0('result_',i,'_',n_chunk,'.RDS')))
  #   i <- i+1
  # }
  # 
  return(result_folder)
}








