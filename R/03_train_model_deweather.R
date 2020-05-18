#' Training a model with deweather package
#'
#' @param station_id 
#' @param data 
#' @param pollutant 
#' @param unit 
#'
#' @return
#' @export
#'
#' @examples
train_model_deweather <- function(data,
                                  training_date_cut,
                                  weather_vars,
                                  add_timestamp_var,
                                  trees,
                                  normalise,
                                  detect_breaks,
                                  samples,
                                  ...){
  
    time_vars <- c("weekday")
    if(add_timestamp_var){
      time_vars <- c(time_vars,"trend")
    }else{
      time_vars <- c(time_vars,"jday")
    }
  
    data_prepared <- data %>%
      mutate(date=as.POSIXct(date)) %>%
      deweather::prepData(add=time_vars)
    
    data_prepared$set = ifelse(caTools::sample.split(data_prepared$value,SplitRatio=0.8),
                                 "training",
                                 "testing")
    data_prepared[data_prepared$date >= training_date_cut,'set'] <- "testing"
    
    data_prepared$weekday_num = as.numeric(data_prepared$weekday) #Otherwise model$pd doesn't work (weekday factors fail)
    
    vars <- c(time_vars, weather_vars)
    vars<-sub("weekday","weekday_num", vars)
    
    model <- deweather::buildMod(dat = data.frame(data_prepared %>% dplyr::filter(set=="training")),
             vars = vars,
             pollutant = "value",
             n.trees = trees,
             sam.size = nrow(data_prepared),
             n.core = 1)
    
    data_prepared$predicted <- predict(model$model, data_prepared, n.trees=model$model$n.trees)
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
    model_light$data<- NULL
    model_light$model$data <- NULL
    model_light$model$trees <- NULL
    
    res <- tibble(model=list(model_light),
             predicted=list(data_prepared %>% dplyr::select(date, set, value, predicted))
      )
    
    if(normalise){
      normalised <- deweather::metSim(model, data_prepared, metVars=weather_vars, n.core=1, B=samples)
      res <- res %>% mutate(normalised=list(normalised))
      
      if(detect_breaks){
        #TODO
      }
    } 
    

    if(add_timestamp_var){
      # Save trend impact (equivalent to weather corrected?)
      res$trend <- list(model$pd %>% dplyr::filter(var=="trend") %>%
        dplyr::mutate(date=lubridate::date_decimal(x)) %>%
        dplyr::ungroup() %>%
        dplyr::select(date, mean, lower, upper))
    }

    res
}