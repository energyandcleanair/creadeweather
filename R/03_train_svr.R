#' Training a model using svr
#'
#'
#' @param data 
#' @param pollutant 
#' @param unit 
#' @param training_date_cut
#'
#' @return
#' @export
#'
#' @examples
train_model_svr <- function(data,
                                  training_date_cut,
                                  weather_vars,
                                  time_vars,
                                  normalise,
                                  samples,
                                  ...){
  
  n_cores <- as.integer(future::availableCores()-1)
  

  # Using deweather to prepare data re time vars
  # Correspondance between our time variables and deweather ones
  # our=deweather
  time_vars_corr <- list(
    "trend"="trend",
    "wday"="weekday",
    "month"="month",
    "week"="week",
    "yday"="jday",
    "hour"="hour"
  )
  
  if(any(!time_vars %in% c(names(time_vars_corr), colnames(data)))){
    stop(paste("Deweather can only create the following timevars:", paste(names(time_vars_corr), collapse=",")))
  }else{
    time_vars <- c(unlist(time_vars_corr[time_vars], use.names=F), setdiff(time_vars,names(time_vars_corr)))
  }
  
  data_prepared <- data %>%
    mutate(date=as.POSIXct(date)) %>%
    deweather::prepData(add=time_vars)
 
  data_prepared[data_prepared$date >= training_date_cut,'set'] <- "prediction"
  data_prepared[data_prepared$date <= training_date_cut,'set'] <- "training" # Actually, gbm will use a fraction of it for validation
  
  # Creating model
  model_svm  <- function(training_data, formula){
    print("Training SVR")
    model_ <- e1071::svm(
      formula = formula,
      data = as.matrix(training_data)
    )
    print("Done")
    return(model_)
  }

  formula_vars <- c(time_vars, weather_vars)
  formula <- reformulate(termlabels=formula_vars,
                         response='value')  
  
  data_prepared <- data_prepared %>%
    dplyr::filter_at(formula_vars, all_vars(!is.na(.))) %>%
    dplyr::filter_at("value", all_vars(!is.na(.)))
  
  
  #----------------
  # Fit model
  #----------------
  model <- model_svm(data_prepared %>% dplyr::filter(set=="training") %>%
                       dplyr::select(c("value", formula_vars)), formula) 
  
  #----------------
  # Predict
  #----------------
  data_prepared$predicted <- predict(model, as.matrix(data_prepared %>% dplyr::select(formula_vars)))
  
  # If fire was part of weather variables
  # We create a no_fire counterfactual
  add_nofire <- any(stringr::str_detect(weather_vars, "fire"))
  if(add_nofire){
    data_prepared_nofire <- data_prepared
    data_prepared_nofire[, grep("fire", names(data_prepared))] <- 0
    data_prepared$predicted_nofire <- predict(model, as.matrix(data_prepared_nofire %>% dplyr::select(formula_vars)))
  }
  
  
  data_prepared$residuals <- data_prepared$predicted - data_prepared$value
  
  data_predict <- data_prepared %>% filter(set=="prediction") %>% filter(!is.na(value))
  
  
  # We only keep 'useful' information to save space
  # Can take several MB per model otherwise
  model_light <- model[c("epsilon",
                         "rho",
                         "tot.nSV")]
  
  model_light$rmse_predict <- Metrics::rmse(data_predict$value, data_predict$predicted)
  model_light$mae_predict <- Metrics::mae(data_predict$value, data_predict$predicted)
  model_light$rsquared_predict <- 1 - sum((data_predict$predicted - data_predict$value)^2) / sum((data_predict$value - mean(data_predict$value))^2)

  data_training <- data_prepared %>% filter(set=="training") %>% filter(!is.na(value))
  model_light$rmse_training <- Metrics::rmse(data_training$value, data_training$predicted)
  model_light$mae_training <- Metrics::mae(data_training$value, data_training$predicted)
  model_light$rsquared_training <- 1 - sum((data_training$predicted - data_training$value)^2) / sum((data_training$value - mean(data_training$value))^2)

  cols <- c("date", "set", "value", "predicted")
  if(add_nofire){
    cols <- c(cols, "predicted_nofire")
  }
  
  res <- tibble(model=list(model_light),
                predicted=list(data_prepared %>% dplyr::select_at(cols) %>% arrange(date)))
   
  res
}
