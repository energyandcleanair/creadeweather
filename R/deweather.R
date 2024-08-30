#' Main Deweathering function
#'
#' @param meas
#' @param poll
#' @param source
#' @param country
#' @param location_id
#' @param location_type
#' @param process_id
#' @param city
#' @param aggregate_level
#' @param date_to
#' @param output
#' @param upload_results
#' @param years_force_refresh
#' @param keep_model
#' @param training_start_trend
#' @param training_end_trend
#' @param training_start_anomaly
#' @param training_end_anomaly
#' @param engine
#' @param lag
#' @param training.fraction
#' @param link
#' @param cv_folds
#' @param save_weather_filename
#' @param read_weather_filename
#' @param weather_vars
#' @param add_fire
#' @param fire_source
#' @param fire_mode
#' @param fire_split_days
#' @param fire_split_regions
#' @param fire_buffer_km
#' @param trajs_parallel
#' @param trajs_height
#' @param use_trajs_cache
#' @param save_trajs_filename
#' @param deweather_process_id 
#' @param ntrainings Number of time models should be trained
#' @param training_excluded_dates 
#' @param weather_for_whole_period 
#' @param weather_sources 
#' @param weather_update_era5 
#' @param fire_split_regions_res 
#' @param upload_fire 
#' @param upload_weather 
#' @param trajs_cores 
#' @param trajs_hours 
#' @param trajs_duration_hour 
#' @param trajs_met_type 
#' @param use_weather_cache 
#'
#' @return
#' @export
#'
#' @examples
deweather <- function(
  
  # AIR QUALITY MEASUREMENTS
  meas=NULL,
  poll=NULL,
  source=NULL,
  country=NULL,
  location_id=NULL,
  location_type=NULL,
  process_id=NULL,
  city=NULL,
  aggregate_level="city",
  date_to=NULL,
  
  
  # PRESET CONFIG
  deweather_process_id=NULL,
  
  
  # DEWEATHERING GENERAL
  output=c("trend"), #c("trend", "anomaly")
  upload_results=F,
  years_force_refresh=NULL,
  keep_model=T,
  training_start_trend="2015-01-01",
  training_end_trend="2099-01-01", # With trend approach, we train over the whole period
  training_start_anomaly="2015-01-01",
  training_end_anomaly="2019-12-31",
  ntrainings=1,

  training_excluded_dates = c(), # A list of dates that MUST not be used for training
  
  # MODEL PARAMETERS
  engine="gbm",
  lag=1,
  training.fraction=0.9, #Used for testing
  link="linear", # 'log' or 'linear'
  cv_folds=3,
  interaction.depth=2,
  learning.rate=0.01,
  trees=10000,
  
  
  # WEATHER
  weather_for_whole_period=T, # Collect weather even where there is no measurement
  save_weather_filename=NULL,
  read_weather_filename=NULL, # Skip weather retrieval, and use cached file instead. Also integrates measurements!
  weather_vars=c('air_temp_min','air_temp_max', 'atmos_pres',
                 'wd', 'ws', 'precip', 'dewpoint_temp', 'pbl_min', 'pbl_max'),
  weather_sources = c('era5'), #also available: NOAA
  weather_update_era5 = T,

  # BIOMASS BURNING
  add_fire=F, #Whether to add it in the model, 
  fire_source="viirs",
  fire_mode="trajectory",
  fire_split_days=F, # whether to split fires by "age" (e.g. 1-day old, 2-day old etc)
  fire_split_regions=NULL, # whether to split fires by region. NULL, "gadm_0", "gadm_1" or "gadm_2"
  fire_split_regions_res='low', # GADM resolution
  fire_buffer_km=50,
  upload_fire=F, # Upload trajs, weather, and meas for biomass burning dashboard
  upload_weather=upload_fire,
  
  
  # TRAJECTORIES
  trajs_parallel=T,
  trajs_cores=parallel::detectCores() - 1,
  trajs_height=10,
  trajs_hours=seq(0,23,4),
  trajs_duration_hour=96,
  trajs_met_type='gdas1',
  use_trajs_cache=T,
  use_weather_cache=T,
  save_trajs_filename=NULL
){
  
  suppressWarnings(try(dotenv::load_dot_env(file = ".env"), silent = T))
  try(readRenviron(".Renviron"))
  
  # Check that environment variables are set
  utils.check_env()

  #----------------------
  # Input presets
  #----------------------
  if (!is.null(deweather_process_id)) {
    # Overwrite parameters by those specified in the process
    deweather_parameters <- process_id_to_parameters(
      process_id = deweather_process_id,
      key = "deweather"
    )

    # Log parameters
    print("Using parameters from process_id:")
    print(deweather_parameters)

    list2env(deweather_parameters, environment())
  }

  #----------------------
  # 0. Get AQ measurements
  #----------------------
  print("0. Getting measurements")
  date_from <- min(c(anomaly=training_start_anomaly,
                     trend=training_start_trend,
                     trend_yday=training_start_trend)[output])
  
  
  date_to <- min(Sys.Date(),
                 as.Date(if(is.null(date_to)) {as.Date('2100-01-01')} else {as.Date(date_to)}))
  
  meas <- get_measurements(meas=meas,
                           poll=poll,
                           country=country,
                           source=source,
                           date_from=date_from,
                           date_to=date_to,
                           city=city,
                           process_id=process_id,
                           aggregate_level=aggregate_level,
                           location_id=location_id,
                           location_type=location_type)
  
  
  

  #---------------------------------------------
  # 1. Get weather data (and fire if demanded)
  #---------------------------------------------
  print("1. Collecting weather")
  
  # We update date_from and date_to based on measurements
  # so as not to query unncessary data
  date_from <- meas %>% dplyr::select(meas) %>% tidyr::unnest(cols=meas) %>% pull(date) %>% min(na.rm=T)
  date_to <- meas %>% dplyr::select(meas) %>% tidyr::unnest(cols=meas) %>% pull(date) %>% max(na.rm=T)
  
  weather <- get_weather(
    location_ids=location_id,
    date_from=date_from,
    date_to=date_to,
    weather_vars=weather_vars,
    weather_sources=weather_sources,
    read_weather_filename=read_weather_filename,
    save_weather_filename=save_weather_filename,
    years=seq(year(date_from),
             year(today())),
    years_force_refresh=years_force_refresh,
    n_per_location=4,
    add_sunshine=F,
    add_fire=add_fire,
    fire_source=fire_source,
    fire_mode=fire_mode,
    fire_buffer_km=fire_buffer_km,
    fire_split_days=fire_split_days,
    fire_split_regions=fire_split_regions,
    fire_split_regions_res=fire_split_regions_res,
    trajs_parallel=trajs_parallel,
    trajs_cores=trajs_cores,
    trajs_height=trajs_height,
    trajs_hours=trajs_hours,
    trajs_duration_hour=trajs_duration_hour,
    trajs_met_type=trajs_met_type,
    use_trajs_cache=use_trajs_cache,
    use_weather_cache=use_weather_cache,
    upload_trajs=upload_fire,
    upload_weather=upload_weather,
    save_trajs_filename=save_trajs_filename,
    update_era5=weather_update_era5
    )
  
  # We might want to collect weather,
  # hence doing this here
  if(is.null(meas)){
    return(NULL)
  }
  
  # update weather_vars based on available weather variables
  weather_vars <- intersect(weather_vars, available_weather_vars(weather = weather))

  
  #-----------------------------------------
  # 1bis. Combine weather and measurements
  #-----------------------------------------
  data <- combine_meas_weather(meas, weather)


  #-----------------------------------------
  # 1ter. List weather variables
  #-----------------------------------------
  if(add_fire){
    fire_vars_pattern <- ifelse(fire_source=="viirs","^fire_frp","^pm25_emission")
    available_vars <- available_weather_vars(weather = weather)
    weather_vars <- unique(c(weather_vars, grep(fire_vars_pattern, available_vars, value=T)))
  }

  #----------------------
  # 2. Preparing data
  #----------------------
  print("2. Preparing data")
  data <- prep_data(
    data = data,
    weather_vars = weather_vars,
    lag = lag
  )

  configs <- create_configs(
    weather_vars = weather_vars,
    add_fire = add_fire,
    output = output,
    engine = engine,
    link = link,
    lag = lag,
    cv_folds = cv_folds,
    interaction.depth = interaction.depth,
    learning.rate = learning.rate,
    trees = trees,
    training.fraction = training.fraction,
    training_start_anomaly = training_start_anomaly,
    training_end_anomaly = training_end_anomaly,
    training_start_trend = training_start_trend,
    training_end_trend = training_end_trend,
    training_excluded_dates = training_excluded_dates,
    keep_model = keep_model,
    trajs_height = trajs_height,
    trajs_hours = trajs_hours,
    fire_source = fire_source,
    fire_buffer_km = fire_buffer_km,
    trajs_met_type = trajs_met_type,
    trajs_height = trajs_height,
    trajs_duration_hour = trajs_duration_hour,
    trajs_hours = trajs_hours,
    ntrainings = ntrainings
  )

  #---------------------------
  # 3. Train models
  #---------------------------
  print("3. Training models")
  print(data)
  
  tryCatch({
    results <- train_configs(data=data,
                             configs=configs)
  }, error = function(e) {
    print(e)
    return(NA)
  })
  
  if(nrow(results)==0 | is.null(results)){
    warnings("Empty results. Returning NA")
    return(NA)
  }


  #--------------------------------------
  # 4. Post-compute / aggregate results
  #--------------------------------------
  print("4. Post-computing")
  results <- postcompute(results = results)


  #--------------------
  # 5. Upload results
  #--------------------
  if (upload_results) {
    print("5. Uploading results")
    upload_results(results, deweather_process_id = deweather_process_id)
  }

  if (add_fire & upload_fire) {
    try({
      upload_fire_results(
        results = results,
        met_type = trajs_met_type,
        duration_hour = trajs_duration_hour,
        fire_source = fire_source,
        fire_split_regions = fire_split_regions,
        trajs_hours = trajs_hours,
        trajs_height = trajs_height,
        fire_buffer_km = fire_buffer_km
      )
    })
  }

  return(results)
}
