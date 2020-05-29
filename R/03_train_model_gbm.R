#' Training a model using standard gmb and comparing observed vs predicted as anomaly
#'
#'
#' @param station_id 
#' @param data 
#' @param pollutant 
#' @param unit 
#' @param training_date_cut
#'
#' @return
#' @export
#'
#' @examples
train_model_gbm <- function(data,
                                  training_date_cut,
                                  weather_vars,
                                  time_vars,
                                  trees,
                                  normalise,
                                  detect_breaks,
                                  samples,
                                  ...){
  
  print(time_vars)
  print(weather_vars)
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
    time_vars <- unlist(time_vars_corr[time_vars], use.names=F)
  }
  
  data_prepared <- data %>%
    mutate(date=as.POSIXct(date)) %>%
    deweather::prepData(add=time_vars)
  
  
 
  data_prepared[data_prepared$date >= training_date_cut,'set'] <- "testing"
  data_prepared[data_prepared$date <= training_date_cut,'set'] <- "training" # Actually, gbm will use a fraction of it for validation
  
  
  
  # Creating model
  model_gbm  <- function(training_data, formula){
    print("Training gbm")
    gbm.fit <- gbm::gbm(
      formula = formula,
      data = training_data,
      cv.folds = 3,
      interaction.depth = 5,
      n.cores = n_cores,
      n.trees = trees, #So far, it seems only up to 300 trees are used
      verbose = FALSE,
      keep.data = FALSE
    )
    print("Done")
    return(gbm.fit)
  }

  formula_vars <- c(time_vars, weather_vars)
  formula <- reformulate(termlabels=formula_vars,
                         response='value')  
  
  data_prepared <- data_prepared %>%
    dplyr::filter_at(formula_vars, any_vars(!is.na(.))) %>%
    dplyr::filter_at("value", all_vars(!is.na(.)))
  
  #----------------
  # Fit model
  #----------------
  model <- model_gbm(data_prepared %>% dplyr::filter(set=="training"), formula) 
  
  #----------------
  # Predict
  #----------------
  data_prepared$predicted <- predict(model, data_prepared)
  data_prepared$residuals <- data_prepared$predicted - data_prepared$value
  
  data_test <- data_prepared %>% filter(set=="testing") %>% filter(!is.na(value))
  model$rmse_test <- rmse(data_test$value, data_test$predicted)
  model$mae_test <- mae(data_test$value, data_test$predicted)
  model$rsquared_test <- 1 - sum((data_test$predicted - data_test$value)^2) / sum((data_test$value - mean(data_test$value))^2)

  data_training <- data_prepared %>% filter(set=="training") %>% filter(!is.na(value))
  model$rmse_training <- rmse(data_training$value, data_training$predicted)
  model$mae_training <- mae(data_training$value, data_training$predicted)
  model$rsquared_training <- 1 - sum((data_training$predicted - data_training$value)^2) / sum((data_training$value - mean(data_training$value))^2)

  # save space
  model_light <- model
  model_light$trees<- NULL
  

  res <- tibble(model=list(model_light),
                predicted=list(data_prepared %>% dplyr::select(date, set, value, predicted))
  )
   
  if(normalise){
    warning("Normalised not managed yet for gbm engine. Use deweather instead if normalisation is your goal")
  }
  
  res
}