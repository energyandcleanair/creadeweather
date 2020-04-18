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

source('99_utils.R')
source('99_plot.R')

output_folder <- file.path('data', '03_train_models', 'output')
if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)

# Check we can find this file (to be copied later on)
org_file <- if(rstudioapi::isAvailable()) rstudioapi::getActiveDocumentContext()$path else '03_train_models.R'


exp_name <- 'lag7_no2_05deg_rmweather'
day_lags <- c(1:7)
n_stations = NULL #50 #You might not want to run every region / poll combination
pollutants <-c('NO2') 

meas_weather <- readRDS('data/02_prep_training/output/meas_w_weather_allpolls_05.RDS')
# only keep those with values in 2020
meas_weather <- meas_weather %>% filter(max(meas_weather$date)>'2020-01-01')

if(!is.null(pollutants)){
  meas_weather <- meas_weather %>% filter(pollutant %in% pollutants)
}

if(!is.null(n_stations)){
  stations <- unique(meas_weather$station_id)[1:n_stations]
  meas_weather <- meas_weather %>% filter(station_id %in% stations)
}

weather_vars_available <- setdiff(colnames(meas_weather$meas_weather[[1]]), c('date','value'))
weather_vars <- c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH', 'pbl_min', 'pbl_max', 'sunshine')
weather_vars_lags <- unlist(lapply(weather_vars, function(x) paste(x,day_lags,sep="_")))


# formula <- reformulate(termlabels=weather_vars_lags,
#                        response='value')
# formula_vars <- vars(all.vars(formula))

# Adding lag
meas_weather_lag <- meas_weather %>% rowwise() %>%
  mutate(meas_weather=list(utils.add_lag(meas_weather, weather_vars, group_cols=c(), day_lags, 'day')))


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
train_row <- function(station_id, data){
  tryCatch({
    data_prepared <- data %>%
      mutate(date=as.POSIXct(date)) %>%
      rmw_prepare_data(na.rm = TRUE)

    rmw_do_all(
      data_prepared,
      variables = c("date_unix","day_julian", "weekday",weather_vars_lags),
      n_trees = 300,
      n_samples = 300,
      verbose = TRUE
    )
  }, error=function(err){
    warning(paste("Station id failed:",station_id,':',err))
    return(NA)})
}

nworkers <- as.integer(future::availableCores() - 1)
models_fitted <- pbmcmapply(train_row,
                            station_id=meas_weather_lag$station_id,
                            data=meas_weather_lag$meas_weather,
                            mc.cores=nworkers,
                            SIMPLIFY=FALSE)


meas_weather_lag$meas_weather <- NULL # saving space
meas_weather_lag$model_fitted <- models_fitted
saveRDS(meas_weather_lag, file=file.path(result_folder,'results.RDS'))



###### Plot results
plot.rmweather.result_rows(meas_weather_lag, rolling_days=7,
                 filepath=file.path(result_folder,paste0(timestamp_str,'_results_7d.pdf')))

plot.rmweather.result_rows(meas_weather_lag, rolling_days=30,
                 filepath=file.path(result_folder,paste0(timestamp_str,'_results_30d.pdf')))


