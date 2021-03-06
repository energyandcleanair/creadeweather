#' Deweathering function
#'
#' @param poll 
#' @param source 
#' @param country 
#' @param location_id 
#' @param city 
#' @param output any combination of "anomaly", "trend", "anomaly_offsetted"
#' @param aggregate_level "station" or "city"
#' @param upload_results T/F Whether to upload results or not
#' @param add_gadm1 T/F Whether to aggregate to GADM1 levels after computation
#' @param add_gadm2 T/F Whether to aggregate to GADM2 levels after computation
#' @param years_force_refresh Force refreshing (i.e. not using cache) of weather data of that year(s)
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
  add_gadm1=F,
  add_gadm2=F,
  years_force_refresh=NULL,
  training_start_trend="2015-01-01",
  training_start_anomaly="2016-12-01",
  training_end_anomaly="2019-11-30",
  lag=1,
  training.fraction=1,
  link="log", # 'log' or 'linear'
  # Fire options
  add_fire=F, #BIOMASS BURNING, WORK IN DEVELOPMENT
  fire_mode="circular",
  add_pbl=T, #INCLUDING PLANETARY BOUNDARY LAYER OR NOT
  save_weather_filename=NULL,
  read_weather_filename=NULL, # Skip weather retrieval, and use cached file instead. Also integrates measurements!
  save_trajs_filename=NULL, # Only used if fire_mode==trajectories and add_fire==T
  keep_model=F
){
  
  suppressWarnings(try(dotenv::load_dot_env(file = ".env")))
  try(readRenviron(".Renviron"))
  
  #----------------------
  # Set parameters
  #----------------------
  training_end_trend <- "2099-01-01"
  
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
  if(is.null(meas)){
    meas <- rcrea::measurements(poll=poll,
                                country=country,
                                location_id=location_id,
                                location_type=location_type,
                                city=city,
                                aggregate_level=aggregate_level,
                                date_from=min(time_vars_output$training_start),
                                source=source,
                                deweathered=F,
                                process_id=process_id,
                                with_metadata=T,
                                with_geometry=T)
  }
  
  # Sometimes, group_by with geometry doesn't work. We split in two steps
  meas_geom <- meas %>% dplyr::distinct(location_id, geometry, timezone)
  
  # For some timezone or summer/winter time related reasons (or bad aggregation?),
  # certain (very few) days in some regions have two measurements,
  # which will ultimately fail due to UNIQUE constraints in Postgres
  # We prevent this now.
  meas <- meas %>% 
    dplyr::group_by(date=lubridate::date(date), location_id, poll, unit, source, timezone, process_id, country) %>%
    dplyr::summarise(value=mean(value, na.rm=T))
  
  meas <- meas %>%
    # dplyr::select(-c(geometry)) %>%
    dplyr::group_by(location_id, poll, unit, source, process_id, country) %>%
    tidyr::nest() %>%
    dplyr::rename(station_id=location_id, meas=data) %>%
    dplyr::ungroup()
  
  if(nrow(meas)==0){
    stop("No measurement found")
  }
  
  meas_sf <- meas %>%
    dplyr::ungroup() %>%
    dplyr::left_join(meas_geom, by=c("station_id"="location_id")) %>%
    dplyr::mutate(geometry=suppressWarnings(sf::st_centroid(geometry))) %>%
    sf::st_as_sf(sf_column_name="geometry", crs = 4326)
  
  #----------------------
  # 1. Add weather
  #----------------------
  print("1. Adding weather")
  if(!is.null(read_weather_filename) && file.exists(read_weather_filename)){
    meas_weather <- readRDS(read_weather_filename)
  }else{
    meas_weather <- creadeweather::collect_weather(meas_sf,
                                                   years=seq(lubridate::year(lubridate::date(min(time_vars_output$training_start))),
                                                             lubridate::year(lubridate::today())),
                                                   years_force_refresh=years_force_refresh,
                                                   add_pbl=add_pbl,
                                                   add_sunshine=F,
                                                   add_fire=add_fire,
                                                   fire_mode=fire_mode,
                                                   save_trajs_filename=save_trajs_filename,
                                                   n_per_station=4
    )
    if(!is.null(save_weather_filename)){
      saveRDS(meas_weather, save_weather_filename)
    }
  }
  
  
  #----------------------
  # 2. Clean data
  #----------------------
  print("2. Cleaning data")
  data <- prep_data(meas_weather=meas_weather)
  
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
  engine <- "gbm"
  weather_vars <- c('air_temp_min','air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH_max')

  if(add_pbl){
    weather_vars <- c(weather_vars,'pbl_min', 'pbl_max')
  }
  
  if(add_fire){
    weather_vars <- c(weather_vars, "fire_frp")
  }
  
  weather_vars <- c(list(weather_vars))
  
  configs <-  tibble() %>%
    tidyr::expand(trees,
                  lag,
                  training.fraction,
                  weather_vars,
                  time_vars_output,
                  engine,
                  link,
                  learning.rate,
                  interaction.depth) %>%
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
        meas_weather=tibble(data),
        weather_vars=weather_vars,
        time_vars=time_vars,
        trees=trees,
        samples=samples,
        interaction.depth=interaction.depth,
        learning.rate=learning.rate,
        lag=lag,
        link=link,
        training.fraction=training.fraction,
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
  
  
  if(add_gadm1 | add_gadm2){
    locs <- rcrea::locations(level="city", source=source, with_meta = T) %>%
      mutate(city=tolower(city_name))
  }
  
  if(add_gadm1){
    results_gadm1 <- post_agg_gadm1(results, locs)
  }
  
  if(add_gadm2){
    results_gadm2 <- post_agg_gadm2(results, locs)
  }
  
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
                                                                            ifelse(add_pbl, "_pbl", ""),
                                                                            ifelse(add_fire, "_fire", ""))))
    
    if(add_gadm1){
      results_gadm1_uploaded <- results_gadm1 %>%
        rowwise() %>%
        rename(output_=output) %>%
        mutate(deweather_process_id=upload_process_meas(process_id, process_deweather, poll, unit, location_id, normalised, source, paste0(output_,"_gbm_lag",lag,"_gadm1",)))
      results_uploaded <- rbind(
        results_uploaded,
        results_anomaly_gadm1_uploaded)
    }
    
    if(add_gadm2){
      results_gadm2_uploaded <- results_gadm2 %>%
        rowwise() %>%
        rename(output_=output) %>%
        mutate(deweather_process_id=upload_process_meas(process_id, process_deweather, poll, unit, location_id, normalised, source, paste0(output_,"_gbm_lag",lag,"_gadm2",)))
      results_uploaded <- rbind(
        results_uploaded,
        results_anomaly_gadm2_uploaded)
    }
    return(results_uploaded)
  }else{
    return(results)
  }
}