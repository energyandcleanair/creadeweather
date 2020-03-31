require(dplyr)
require(purrr)
require(sf)
require(tibble)
require(lubridate)
require(ggplot2)

exp_name <- 'test_1'
test_frac <- 0.1
day_lags <- c(1:3)

meas_weather <- readRDS('data/02_prep_training/output/meas_weather_gadm1.RDS')

models <- list(model_gbm=function(training_data, formula){
    print("Training gbm")
    gbm.fit <- gbm::gbm(
      formula = formula,
      data = training_data,
      cv.folds = 5,
      n.trees = 1500,
      verbose = FALSE
    )
    print("Done")
    return(gbm.fit)
})

colnames(meas_weather$meas[[1]])

formula <- reformulate(termlabels=c('Psurf_f_inst', paste('Psurf_f_inst',day_lags,sep="_"),
                                    'Qair_f_inst', paste('Qair_f_inst',day_lags,sep="_"),
                                    'Rainf_tavg', paste('Rainf_tavg',day_lags,sep="_"),
                                    'Tair_f_inst', paste('Tair_f_inst',day_lags,sep="_"),
                                    'Wind_f_inst', paste('Wind_f_inst',day_lags,sep="_")
                                    ),
                       response='value')
formula_vars <- vars(all.vars(formula))
weather_vars <- c('Psurf_f_inst','Qair_f_inst','Rainf_tavg','Tair_f_inst','Wind_f_inst')

# Adding lag
meas_weather_lag <- meas_weather %>% rowwise() %>%
  mutate(meas=list(utils.add_lag(meas, weather_vars, group_cols=c(), day_lags, 'day')))
         
# Keep only full observations
meas_weather_lag <- meas_weather_lag %>%
  mutate(meas=list(meas %>% filter_at(c(formula_vars, vars(value)), all_vars(!is.na(.)))))

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


#----------------
# Train models
#----------------
# Split training / validation
input_data <- meas_weather_lag %>% rowwise() %>%
  mutate(meas = list(meas %>% mutate(id = row_number()))) %>%
  mutate(training = list(dplyr::sample_frac(meas, 1-test_frac))) %>% 
  mutate(testing = list(anti_join(meas, training, by='id')))

# Adding models to tibble (each model is applied to each region, poll combination)
model_names <- if (!is.null(names(models))) names(models) else seq_along(models)
models_df <- tibble(model_name=names(models), model=models)
input_data <- input_data %>% tidyr::crossing(models_df)

output_data <- input_data[1:50,] %>%
  mutate(model_fitted=purrr::map2(training, model, purrr::possibly(~.y(.x %>% select_at(formula_vars), formula), otherwise = NA, quiet = FALSE)))


#----------------
# Predict
#----------------
output_data <- output_data %>% mutate(training=purrr::map2(training, model_fitted, purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x)), otherwise = NA)))

output_data <- output_data %>% mutate(testing=purrr::map2(testing, model_fitted, purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x)), otherwise = NA)))

# Also predict on whole dataset for plotting purposes
output_data <- output_data %>% mutate(meas=purrr::map2(meas, model_fitted, purrr::possibly(~ .x %>% mutate(predicted=predict(.y, .x)), otherwise = NA)))


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
saveRDS(output_data, file.path(result_folder, paste0(timestamp_str,'_results.RDS')))

roll_plot <- function(raw, n_days){
  result <- raw %>%
    dplyr::mutate(date=lubridate::floor_date(date, unit = 'day')) %>%
    dplyr::group_by(gadm1_id, AirPollutant, rsq, rsq_test, mae, mae_test, date, type) %>%
    dplyr::summarise(value=mean(value, na.rm = T)) %>% dplyr::ungroup() %>%
    dplyr::group_by(gadm1_id, AirPollutant, rsq, rsq_test, mae, mae_test, type) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(value=zoo::rollapply(value, width=n_days,
                                       FUN=function(x) mean(x, na.rm=TRUE), align='right',fill=NA)) %>%
    dplyr::ungroup()
  
  return(result)
}

plot_data <- output_data %>%
  dplyr::select(gadm1_id, AirPollutant, meas, rsq, rsq_test, mae, mae_test) %>%
  filter(!is.na(meas)) %>%
  tidyr::unnest(cols=meas) %>%
  dplyr::select(gadm1_id, AirPollutant, date, rsq, rsq_test, mae, mae_test, value, predicted) %>%
  tidyr::gather("type", "value", -c(gadm1_id, AirPollutant, date, rsq, rsq_test, mae, mae_test)) %>%
  roll_plot(7)


ggplot(plot_data %>% filter(date>='2020-01-01'), aes(x=date, y=value, colour=type)) +
  facet_grid(gadm1_id ~ AirPollutant, scales='free') +
  geom_line()
