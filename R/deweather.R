#' Deweathering function
#'
#' @param meas Observation measurements. If NULL, the function will use rcrea::measurements
#' and the parameters below (`poll`, `source`, `country`, `location_id`, and `city`) to retrieve measurements.
#' @param poll 
#' @param source 
#' @param country 
#' @param location_id 
#' @param city 
#' @param output any combination of "anomaly", "trend", "anomaly_offsetted"
#' @param aggregate_level "station" or "city"
#' @param upload_results T/F Whether to upload results or not
#' @param years_force_refresh Force refreshing (i.e. not using cache) of weather data of that year(s)

#' @param location_type 
#' @param process_id 
#' @param training_start_trend 
#' @param training_start_anomaly 
#' @param training_end_anomaly 
#' @param lag 
#' @param training.fraction 
#' @param link 
#' @param add_fire 
#' @param fire_mode circular, oriented, trajectory or dispersion
#' @param fire_duration_hour 
#' @param fire_buffer_km 
#' @param fire_vars_pattern pattern for fire variable names used in model
#' @param add_pbl 
#' @param trajs_height receptor height for trajectories in meter.
#' If null, pbl average will be considered.
#' @param save_weather_filename 
#' @param read_weather_filename 
#' @param save_trajs_filename 
#' @param keep_model 
#'
#' @return
#' @export
#'
deweather <- function(
  meas=NULL,
  poll=NULL,
  source=NULL,
  country=NULL,
  location_id=NULL,
  location_type=NULL,
  process_id=NULL,
  city=NULL,
  output=c("anomaly"), #c("trend","anomaly")
  aggregate_level="city",
  upload_results=T,
  years_force_refresh=NULL,
  engine="gbm",
  training_start_trend="2015-01-01",
  training_start_anomaly="2016-12-01",
  training_end_anomaly="2019-11-30",
  lag=1,
  add_pbl=T, #INCLUDING PLANETARY BOUNDARY LAYER OR NOT
  training.fraction=0.9, #Used for testing
  link="linear", # 'log' or 'linear'
  cv_folds=3,
  
  
  #Biomass burning [WIP]
  add_fire=F, #Whether to add it in the model, 
  calc_fire=add_fire, #Whether to calculate fire numbers
  fire_source="viirs",
  fire_vars_pattern=ifelse(fire_source=="viirs", "^fire_frp","^pm25_emission"),
  fire_mode="circular",
  fire_split_days=F, # whether to split fires by "age" (e.g. 1-day old, 2-day old etc)
  fire_split_regions=NULL, # whether to split fires by region. NULL, "gadm_0" or "gadm_1"
  fire_duration_hour=72, # For trajectories only
  fire_buffer_km=10,
  
  # Trajectories
  trajs_parallel=T,
  trajs_height=NULL,
  use_trajs_cache=T,
  save_trajs_filename=NULL,
  
  save_weather_filename=NULL,
  read_weather_filename=NULL, # Skip weather retrieval, and use cached file instead. Also integrates measurements!
  
  keep_model=T
){
  
  suppressWarnings(try(dotenv::load_dot_env(file = ".env"), silent = T))
  try(readRenviron(".Renviron"))
  
  #----------------------
  # Input check
  #----------------------
  if(!calc_fire & add_fire){
    warning('We need to calculate fire numbers in order to account for it. Setting calc_fire=T')
    calc_fire=T
  }
  
  
  #----------------------
  # Set parameters
  #----------------------
  training_end_trend <- "2099-01-01" # With trend approach, we train over the whole period
  
  time_vars_output <- tibble(
    time_vars=c(list(c('yday')), list(c()), list(c('trend'))),
    output=c('anomaly_yday', 'anomaly', 'trend'),
    training_end=c(training_end_anomaly, training_end_anomaly, training_end_trend),
    training_start=c(training_start_anomaly, training_start_anomaly, training_start_trend)
  ) %>%
    filter(output %in% !!output)
  
  
  #----------------------
  # 0. Get measurements
  #----------------------
  print("0. Getting measurements")
  meas <- get_measurements(meas=meas,
                           poll=poll,
                           country=country,
                           source=source,
                           date_from=min(time_vars_output$training_start),
                           city=city,
                           process_id=process_id,
                           aggregate_level=aggregate_level,
                           location_id=location_id,
                           location_type=location_type)
  
  #----------------------
  # 1. Get weather
  #----------------------
  print("1. Adding weather")
  if(!is.null(read_weather_filename) && file.exists(read_weather_filename)){
    weather <- read_weather(read_weather_filename)
  }else{
    weather <- collect_weather(meas,
                               years=seq(lubridate::year(lubridate::date(min(time_vars_output$training_start))),
                                                       lubridate::year(lubridate::today())),
                               years_force_refresh=years_force_refresh,
                               n_per_station=4,
                               add_pbl=add_pbl,
                               add_sunshine=F,
                               add_fire=calc_fire,
                               fire_source=fire_source,
                               fire_mode=fire_mode,
                               fire_duration_hour=fire_duration_hour,
                               fire_buffer_km=fire_buffer_km,
                               fire_split_days=fire_split_days,
                               fire_split_regions=fire_split_regions,
                               trajs_parallel=trajs_parallel,
                               trajs_height=trajs_height,
                               use_trajs_cache=use_trajs_cache,
                               save_trajs_filename=save_trajs_filename
    )
    if(!is.null(save_weather_filename)){
      saveRDS(weather, save_weather_filename)
    }
  }
  
  #-----------------------------------------
  # 1bis. Combine weather and measurements
  #-----------------------------------------
  data <- combine_meas_weather(meas, weather)
  
  #-----------------------------------------
  # 1ter. List weather variables
  #-----------------------------------------
  weather_vars <- c('air_temp_min','air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'precip', 'RH_max')
  if(add_pbl){
    weather_vars <- c(weather_vars,'pbl_min', 'pbl_max')
  }
  
  if(add_fire){
    if(fire_source=="viirs"){
      weather_vars <- c(weather_vars, grep(fire_vars_pattern, names(weather), value=T))
    }
    if(fire_source=="gfas"){
      weather_vars <- c(weather_vars, grep("^pm25_emission.*", names(weather), value=T))
    }
  }
  
  #----------------------
  # 2. Clean data
  #----------------------
  print("2. Cleaning data")
  data <- prep_data(data=data,
                    weather_vars=weather_vars,
                    lag=lag)
  
  #----------------------
  # 3. Train models
  #----------------------
  print("3. Training models")
  normalise <- F
  detect_breaks <- F
  trees <- 10000
  samples <- 100
  interaction.depth <- c(2)
  learning.rate <- c(0.01)

  weather_vars_list <- c(list(unique(weather_vars)))
  
  configs <-  tibble() %>%
    tidyr::expand(trees,
                  lag,
                  training.fraction,
                  weather_vars=weather_vars_list,
                  time_vars_output,
                  engine,
                  link,
                  learning.rate,
                  interaction.depth,
                  cv_folds=cv_folds) %>%
    rowwise() %>%
    mutate(process_deweather=
             gsub("'","\"",paste0("{",
                                  "'engine':'",engine,"',",
                                  "'trees':'",trees,"',",
                                  "'learning.rate':'",learning.rate,"',",
                                  "'interaction.depth':'",interaction.depth,"',",
                                  "'training.fraction':'",training.fraction,"',",
                                  "'lag':'",lag,"',",
                                  "'training_start':'",training_start,"',",
                                  "'training_end':'",training_end,"',",
                                  "'time_vars':['",paste0(time_vars,collapse="','"),"'],",
                                  "'weather_vars':['",paste0(weather_vars,collapse="','"),"'],",
                                  "'link':'",link,"',",
                                  "'output':'",output,"'",
                                  "}")
             )
    )
  
  results_nested <- configs %>%
    rowwise() %>%
    mutate(
      result=list(train_models(
        engine=engine,
        data=tibble(data),
        weather_vars=weather_vars,
        time_vars=time_vars,
        trees=trees,
        samples=samples,
        interaction.depth=interaction.depth,
        learning.rate=learning.rate,
        lag=lag,
        link=link,
        training.fraction=training.fraction,
        cv_folds=cv_folds,
        normalise=normalise,
        detect_breaks=detect_breaks,
        training_date_cut=training_end)
      )) %>%
    rowwise() %>%
    filter(any(!is.na(result))) %>%
    ungroup()
  
  if(nrow(results_nested)==0){
    warnings("Empty results. Returning NA")
    return(NA)
  }
  
  
  #--------------------------------------
  # 4. Post-compute / aggregate results
  #--------------------------------------
  print("4. Post-computing")
  
  results <- creadeweather::post_compute(results_nested=results_nested,
                                         output=output,
                                         add_fire=add_fire,
                                         keep_model=keep_model)
  
  #--------------------
  # 5. Upload results
  #--------------------
  
  if(upload_results){
    print("5. Uploading results")  
    prcs <- rcrea::processes() %>% dplyr::collect()
    process_id_to_filter_type <- function(process_id, prcs){
      prcs %>%
        filter(id == !!process_id) %>%
        pull(filter_type)
    }
    
    processes <- results %>% distinct(process_id, process_deweather)
    
    results_uploaded <- results %>%
      rowwise() %>%
      rename(output_=output) %>%
      mutate(deweather_process_id=upload_process_meas(process_id, process_deweather, poll, unit, location_id, normalised, source,
                                                      preferred_name=paste0(output_,
                                                                            "_gbm_lag",lag,
                                                                            "_",aggregate_level,
                                                                            ifelse(!is.null(location_type), paste0("_",location_type), ""),
                                                                            "_",process_id_to_filter_type(process_id, prcs),
                                                                            ifelse(add_fire, "_fire", ""),
                                                                            ifelse(!add_pbl, "_nopbl", ""))))
    
    return(results_uploaded)
  }else{
    return(results)
  }
}