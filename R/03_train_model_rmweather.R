#' Training a model with rmweather package
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
train_model_rmweather <- function(data,
                                  training_date_cut,
                                  weather_vars,
                                  time_vars,
                                  trees,
                                  normalise,
                                  detect_breaks,
                                  samples,
                                  ...){

    n_cores <- as.integer(future::availableCores()-1)
  
    # Correspondance between our time variables and rmweather ones
    # our=deweather
    time_vars_corr <- list(
      "trend"="date_unix",
      "wday"="weekday",
      "yday"="day_julian",
      "hour"="hour"
    )
    if(any(!time_vars %in% c(names(time_vars_corr), colnames(data)))){
      stop(paste("Rmweather can only create the following timevars:", paste(names(time_vars_corr), collapse=",")))
    }else{
      time_vars <- unlist(time_vars_corr[time_vars], use.names=F)
    }
    
    
    data_prepared <- data %>%
      mutate(date=as.POSIXct(date)) %>%
      rmw_prepare_data(na.rm = TRUE)
    
    data_prepared[data_prepared$date >= training_date_cut,'set'] <- "testing"
    
    variables <- c(time_vars, weather_vars)
    
    model <- rmw_train_model(
      df=data_prepared,
      variables = variables,
      n_trees = trees,
      verbose = F,
      n_cores = n_cores
    )
    
    data_prepared$predicted <- rmw_predict(model, data_prepared)
    data_test <- data_prepared %>% filter(set=="testing")
    model$rmse_test <- rmse(data_test$value, data_test$predicted)
    model$mae_test <- mae(data_test$value, data_test$predicted)
    model$rsquared_test <- 1 - sum((data_test$predicted - data_test$value)^2) / sum((data_test$value - mean(data_test$value))^2)
    
    data_training <- data_prepared %>% filter(set=="training")
    model$rmse_training <- rmse(data_training$value, data_training$predicted)
    model$mae_training <- mae(data_training$value, data_training$predicted)
    model$rsquared_training <- 1 - sum((data_training$predicted - data_training$value)^2) / sum((data_training$value - mean(data_training$value))^2)
    
    # save space
    model_light <- model
    model_light$forest<- NULL
    model_light$observations <- NULL
    model_light$inbag.counts <- NULL
    model_light$predictions <- NULL
    
    res <- tibble(model=list(model_light),
             predicted=list(data_prepared %>% dplyr::select(date, set, value, predicted))
      )
    
    if(normalise){
      normalised <- rmw_normalise(model=model,
                                  df=data_prepared,
                                  variables=weather_vars,
                                  n_samples=samples, n_cores=1)
      res <- res%>% mutate(normalised=list(normalised))
      
      if(detect_breaks){
        breakpoints_ <- rmw_find_breakpoints(normalised)
        res <- res %>% mutate(breakpoints=list(breakpoints_))
      }
    } 
    
    if("date_unix" %in% time_vars){
      # Save trend impact (equivalent to weather corrected?)
      res$trend <- list(rmw_partial_dependencies(model, data_prepared, "date_unix") %>%
                          dplyr::mutate(date=as.POSIXct.numeric(value, origin="1970-01-01")) %>%
                          dplyr::rename(mean=partial_dependency) %>%
                          dplyr::select(-c(variable, value)))
    }
    
    res
}
