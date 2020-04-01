require(dplyr)
require(purrr)
require(sf)
require(tibble)
require(lubridate)
require(ggplot2)
source('99_utils.R')

exp_name <- 'lag7'
test_frac <- 0.1
day_lags <- c(1:7)
n_regions = NULL #50 #You might not want to run every region / poll combination

meas_weather <- readRDS('data/02_prep_training/output/meas_weather_gadm1.RDS')

models <- list(model_gbm=function(training_data, formula){
    print("Training gbm")
    gbm.fit <- gbm::gbm(
      formula = formula,
      data = training_data,
      cv.folds = 5,
      n.trees = 800, # So farm, it seems only up to 500 trees are used
      verbose = FALSE,
      keep.data = FALSE
    )
    print("Done")
    return(gbm.fit)
})

weather_vars_available <- setdiff(colnames(meas_weather$meas_weather[[1]]), c('date','value'))
weather_vars <- c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd_factor', 'ws_max', 'ceil_hgt', 'visibility',
                  'precip', 'RH', 'sunshine')
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
result_folder <- file.path('data','03_train_models', 'output', paste(timestamp_str,exp_name,sep='_'))
dir.create(result_folder)

# Save script file
org_file <- rstudioapi::getActiveDocumentContext()$path
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
input_data <- meas_weather_lag %>% rowwise() %>%
  mutate(meas_weather = list(meas_weather %>% mutate(id = row_number()))) %>%
  mutate(training = list(dplyr::sample_frac(meas_weather, 1-test_frac))) %>% 
  mutate(testing = list(anti_join(meas_weather, training, by='id')))

# Adding models to tibble (each model is applied to each region, poll combination)
model_names <- if (!is.null(names(models))) names(models) else seq_along(models)
models_df <- tibble(model_name=names(models), model=models)
input_data <- input_data %>% tidyr::crossing(models_df)

n_poll = length(unique(meas_weather$pollutant))
n_trainings = ifelse(!is.null(n_regions),n_regions * n_poll *length(models),nrow(input_data)) 
output_data <- input_data[1:n_trainings,] %>%
  mutate(model_fitted=purrr::map2(training, model, purrr::possibly(~.y(.x %>% select_at(formula_vars), formula), otherwise = NA, quiet = FALSE)))

#----------------
# Predict
#----------------
output_data <- output_data %>% mutate(training=purrr::map2(training, model_fitted, purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x)), otherwise = NA)))

output_data <- output_data %>% mutate(testing=purrr::map2(testing, model_fitted, purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x)), otherwise = NA)))

# Also predict on whole dataset for plotting purposes
output_data <- output_data %>% mutate(meas_weather=purrr::map2(meas_weather, model_fitted, purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x)), otherwise = NA)))

#---------------
# Post compute
#---------------
rsq <- function(x,y) cor(x, y) ^ 2
output_data <- output_data %>% mutate(training=purrr::map(training, purrr::possibly(~ .x %>% mutate(residuals=predicted-value), otherwise = NA)))
output_data <- output_data %>% mutate(testing=purrr::map(testing,  purrr::possibly(~ .x %>% mutate(residuals=predicted-value), otherwise = NA)))
output_data <- output_data %>% mutate(rmse=purrr::map_dbl(training, purrr::possibly(~ Metrics::rmse(.x$value, .x$predicted), otherwise = NA)))
output_data <- output_data %>% mutate(rmse_test=purrr::map_dbl(testing, purrr::possibly(~ Metrics::rmse(.x$value, .x$predicted), otherwise = NA)))
output_data <- output_data %>% mutate(mae=purrr::map_dbl(training, purrr::possibly(~ Metrics::mae(.x$value, .x$predicted), otherwise = NA)))
output_data <- output_data %>% mutate(mae_test=purrr::map_dbl(testing, purrr::possibly(~ Metrics::mae(.x$value, .x$predicted), otherwise = NA)))
output_data <- output_data %>% mutate(rsq=purrr::map_dbl(training, purrr::possibly(~ rsq(.x$value, .x$predicted), otherwise = NA)))
output_data <- output_data %>% mutate(rsq_test=purrr::map_dbl(testing, purrr::possibly(~ rsq(.x$value, .x$predicted), otherwise = NA)))

#-----------------------
# Save and plot results
#-----------------------
saveRDS(output_data %>% dplyr::select(-c(training, testing)), file.path(result_folder, paste0(timestamp_str,'_results.RDS')))


plot.output_data(output_data, rolling_days=7,
                              filepath=file.path(result_folder,paste0(timestamp_str,'_results_plotrows_7d.pdf')))

plot.output_data(output_data, rolling_days=7,
                 filepath=file.path(result_folder,paste0(timestamp_str,'_results_plotrows_30d.pdf')))

plot.output_map(output_data, result_folder=result_folder, timestamp_str = timestamp_str)


