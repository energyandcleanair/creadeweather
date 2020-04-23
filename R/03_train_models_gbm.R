
train_models_rmweather <- function(meas_weather, pollutants, exp_suffix=NULL){
    
  output_folder <- file.path('data', '03_train_models', 'output')
  if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
  
  # Check we can find this file (to be copied later on)
  org_file <- if(rstudioapi::isAvailable()) rstudioapi::getActiveDocumentContext()$path else '03_train_models.R'
  print(org_file)
  
  exp_name <- 'lag7_no2_novisibility_05deg_depth5'
  test_frac <- 0.1
  day_lags <- c(1:7)
  n_stations = NULL #50 #You might not want to run every region / poll combination
  pollutants <-c('NO2') 
  
  meas_weather <- readRDS('data/02_prep_training/output/meas_w_weather_allpolls_05.RDS')
  
  # only keep those with values in 2020
  # meas_weather <- meas_weather %>% filter(max(meas_weather$date)>'2020-01-01')
  
  if(!is.null(pollutants)){
    meas_weather <- meas_weather %>% filter(pollutant %in% pollutants)
  }
  
  if(!is.null(n_stations)){
    stations <- unique(meas_weather$station_id)[1:n_stations]
    meas_weather <- meas_weather %>% filter(station_id %in% stations)
  }
  
  models <- list(model_gbm=function(training_data, formula){
    print("Training gbm")
    gbm.fit <- gbm::gbm(
      formula = formula,
      data = training_data,
      cv.folds = 3,
      interaction.depth = 5,
      n.cores = 1,
      n.trees = 300, # So far, it seems only up to 300 trees are used
      verbose = FALSE,
      keep.data = FALSE
    )
    print("Done")
    return(gbm.fit)
  })
  
  weather_vars_available <- setdiff(colnames(meas_weather$meas_weather[[1]]), c('date','value'))
  weather_vars <- c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH', 'pbl_min', 'pbl_max', 'sunshine')
  
  weather_vars_lags <- unlist(lapply(weather_vars, function(x) paste(x,day_lags,sep="_")))
  formula <- reformulate(termlabels=weather_vars_lags,
                         response='value')
  formula_vars <- vars(all.vars(formula))
  
  # Adding lag
  meas_weather_lag <- meas_weather %>% rowwise() %>%
    mutate(meas_weather=list(utils.add_lag(meas_weather, weather_vars, group_cols=c(), day_lags, 'day')))
  
  # Keep only observations with a concentration and some weather data
  # GBM can deal with incomplete observations -> we use any_vars rather than all_vars
  meas_weather_lag <- meas_weather_lag %>%
    mutate(meas_weather=list(meas_weather %>% filter_at(weather_vars_lags, any_vars(!is.na(.)))))
  meas_weather_lag <- meas_weather_lag %>%
    mutate(meas_weather=list(meas_weather %>% filter(!is.na(value))))
  
  
  ### Result folder
  # Create results folder
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  result_folder <- file.path(output_folder, paste(timestamp_str,exp_name,sep='_'))
  dir.create(result_folder)
  
  # Save script file
  file.copy(org_file, result_folder, overwrite = T)
  file.rename(from = file.path(result_folder, basename(org_file)),
              to = file.path(result_folder, paste0(timestamp_str, '_', tools::file_path_sans_ext(basename(org_file)),'.R')))
  
  # Map count of inputs (to compare with output and see where models failed)
  plot.map_count(meas_weather_lag, folder=result_folder,
                 title='Number of full measurements / weather observations',
                 meas_col = 'meas_weather')
  
  #----------------
  # Train models
  #----------------
  # Split training / validation
  # data <- meas_weather_lag %>% rowwise() %>%
  #   mutate(meas_weather = list(meas_weather %>% mutate(id = row_number()))) %>%
  #   mutate(training = list(dplyr::sample_frac(meas_weather %>% filter(date<="2020-01-01"), 1-test_frac))) %>% 
  #   mutate(testing = list(anti_join(meas_weather %>% filter(date<="2020-01-01"), training, by='id')))
  
  data <- meas_weather_lag %>% rowwise() %>%
    mutate(meas_weather = list(meas_weather %>% mutate(id = row_number()))) %>%
    mutate(training = list(dplyr::sample_frac(meas_weather, 1-test_frac))) %>% 
    mutate(testing = list(anti_join(meas_weather, training, by='id')))
  
  # Adding models to tibble (each model is applied to each region, poll combination)
  model_names <- if (!is.null(names(models))) names(models) else seq_along(models)
  models_df <- tibble(model_name=names(models), model=models)
  data <- data %>% tidyr::crossing(models_df)
  
  train_row <- function(station_id, model, training){
    tryCatch({
      model(training %>% select_at(formula_vars), formula) 
    }, error=function(err){
      warning(paste("Station id failed:",station_id,':',err))
      return(NA)})
  }
  
  nworkers <- as.integer(future::availableCores() / 2)
  models_fitted <- pbmcmapply(train_row,
                              station_id=data$station_id,
                              model=data$model,
                              training=data$training,
                              mc.cores=nworkers,
                              SIMPLIFY=FALSE)
  # models_fitted <- pbmclapply(seq(nrow(data)), train_row, mc.cores=nworkers)
  # models_fitted <- pblapply(seq(nrow(data)), train_row)
  data$model_fitted <- unname(models_fitted)
  models_fitted <- NULL
  
  #----------------
  # Predict
  #----------------
  data <- data %>%
    mutate(training=purrr::map2(
      training, model_fitted,
      purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x),
                                      residuals=predicted-value),
                      otherwise = NA)))
  
  data <- data %>%
    mutate(testing=purrr::map2(
      testing, model_fitted,
      purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x),
                                      residuals=predicted-value),
                      otherwise = NA)))
  
  # Also predict on whole dataset for plotting and residual analysis purposes
  data <- data %>%
    mutate(meas_weather=purrr::map2(
      meas_weather, model_fitted,
      purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x),
                                      residuals=predicted-value),
                      otherwise = NA)))
  
  #---------------
  # Post compute
  #---------------
  rsq <- function(x,y) cor(x, y) ^ 2
  data <- data %>% mutate(rmse=purrr::map_dbl(training, purrr::possibly(~ Metrics::rmse(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(rmse_test=purrr::map_dbl(testing, purrr::possibly(~ Metrics::rmse(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(mae=purrr::map_dbl(training, purrr::possibly(~ Metrics::mae(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(mae_test=purrr::map_dbl(testing, purrr::possibly(~ Metrics::mae(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(rsq=purrr::map_dbl(training, purrr::possibly(~ rsq(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(rsq_test=purrr::map_dbl(testing, purrr::possibly(~ rsq(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(mase=purrr::map_dbl(training, purrr::possibly(~ Metrics::mase(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(mase_test=purrr::map_dbl(testing, purrr::possibly(~ Metrics::mase(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(mrae=purrr::map_dbl(training, purrr::possibly(~ Metrics::mrae(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(mrae_test=purrr::map_dbl(testing, purrr::possibly(~ Metrics::mrae(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(me=purrr::map_dbl(training, purrr::possibly(~ Metrics::me(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(me_test=purrr::map_dbl(testing, purrr::possibly(~ Metrics::me(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(mpe=purrr::map_dbl(training, purrr::possibly(~ Metrics::mpe(.x$value, .x$predicted), otherwise = NA)))
  data <- data %>% mutate(mpe_test=purrr::map_dbl(testing, purrr::possibly(~ Metrics::mpe(.x$value, .x$predicted), otherwise = NA)))
  
  
  #-----------------------
  # Save and plot results
  #-----------------------
  saveRDS(data %>% dplyr::select(-c(training, testing)), file.path(result_folder, paste0(timestamp_str,'_results.RDS')))
  
  
  plot.output_data(data, rolling_days=7,
                   filepath=file.path(result_folder,paste0(timestamp_str,'_results_plotrows_7d.pdf')))
  
  plot.output_data(data, rolling_days=30,
                   filepath=file.path(result_folder,paste0(timestamp_str,'_results_plotrows_30d.pdf')))
  
  plot.output_map(data, result_folder=result_folder, timestamp_str = timestamp_str,
                  meas_col='rsq_test', title=expression(paste(R^{2}, " validation")))
  
  
  plot.output_result_quality(data, result_folder=result_folder, timestamp_str = timestamp_str)
  
  ## Quality assessment
  data$criteria <- (data$rsq_test>=0.5)
  breaks <- seq(0,1,0.2)
  labels <- c()
  for(i in seq(length(breaks)-1)){ labels[[i]] <- paste(breaks[i],breaks[i+1],sep="-")}
  data$criteria_c <- cut(data$rsq_test, breaks=breaks, labels=labels)
  plot.output_map(data, result_folder=result_folder, timestamp_str=timestamp_str, title='', meas_col='criteria',
                  scale=scale_color_crea_d())
  
  
  ### Residuals analysis: how does March 2020 compare with March 2015-2019
  average_over_yearly_periods <- function(tbl, meas_col, years, doys){
    if(is.na(tbl)) return(NA)
    # doys: day of years
    (tbl %>% filter(lubridate::year(date) %in% years,
                    lubridate::yday(date) %in% doys) %>%
        group_by() %>%
        summarise_at(c(meas_col), mean, na.rm = TRUE))[[meas_col]][[1]]
  }
  
  data <- data %>% rowwise() %>% 
    mutate(march_res_before=average_over_yearly_periods(meas_weather, 'residuals', years=c(2015:2019), doys=c(62:93)),
           march_res_after=average_over_yearly_periods(meas_weather, 'residuals', years=c(2020), doys=c(62:93)),
           value_mean=if(is.na(meas_weather)) NA else mean(meas_weather[['value']], na.rm = T)) %>%
    mutate(march_res_comparison = -(march_res_after - march_res_before)/value_mean,
           march_res_after_neg=(march_res_after/value_mean)*-1)
  
  data <- data %>% rowwise() %>%
    mutate(march2020_value=average_over_yearly_periods(meas_weather, 'value', years=c(2020), doys=c(62:93)),
           march2020_predicted=average_over_yearly_periods(meas_weather, 'predicted', years=c(2020), doys=c(62:93))) %>%
    mutate(march2020_ratio=-1+march2020_value/march2020_predicted)
  
  data$march2020_ratio_c <- cut(data$march2020_ratio, labels=c("Decrease >100% (sic)",
                                                               "Decrease 40-100%",
                                                               "Decrease 20-40%",
                                                               "Decrease 10-20%",
                                                               "Decrease 5-10%",
                                                               "Decrease 0-5%",
                                                               "Increase 0-5%",
                                                               "Increase 5-10%",
                                                               "Increase 10-100%",
                                                               "Increase 20-40%",
                                                               "Increase 40-100%",
                                                               "Increase > 100%"
  ),
  breaks=c(-Inf,-1,-0.4,-0.2,-0.1,-0.05,0,0.05,0.1,0.2,0.4,1,+Inf))
  
  data$march2020_ratio_c <- reorder(data$march2020_ratio_c, desc(data$march2020_ratio_c))
  
  plot.output_map(data %>% filter(criteria==T), result_folder=result_folder, timestamp_str = timestamp_str,
                  meas_col='march2020_ratio_c', title="March 2020 - Value vs Predicted",
                  scale=scale_colour_brewer(palette='RdBu', na.value="grey"),
                  labs=labs(fill=""))
  
  
  plot.output_map(data, result_folder=result_folder, timestamp_str = timestamp_str,
                  meas_col='march_res_comparison', title="Relative March 2020 residuals vs March 2015-2019 residuals (blue: value is lower than predicted)",
                  scale=scale_fill_distiller(palette='RdBu', limits=c(-1,1), breaks=c(-1,-0.05,0,0.05,1), na.value="grey"))
}

