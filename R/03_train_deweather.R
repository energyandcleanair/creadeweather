#' Training a model with deweather package
#'
#' @param data 
#' @param pollutant 
#' @param unit 
#'
#' @return
#' @export
#'
#' @examples
train_deweather <- function(data,
                                  training_end,
                                  weather_vars,
                                  time_vars,
                                  trees,
                                  normalise,
                                  detect_breaks,
                                  samples,
                                  ...){
    
    n_cores <- as.integer(future::availableCores()-1)
  
    # Correspondance between our time variables and deweather ones
    # our=deweather
    time_vars_corr <- list(
      "date_unix"="trend",
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
    
    data_prepared$set = ifelse(caTools::sample.split(data_prepared$value,SplitRatio=0.8),
                                 "training",
                                 "prediction")
    data_prepared[data_prepared$date >= training_end,'set'] <- "prediction"
    vars <- c(time_vars, weather_vars)
    
    if("weekday" %in% time_vars){
      data_prepared$weekday_num = as.numeric(data_prepared$weekday) #Otherwise model$pd doesn't work (weekday factors fail)
      vars<-sub("weekday","weekday_num", vars)
    }

    model <- deweather::buildMod(dat = data.frame(data_prepared %>% dplyr::filter(set=="training")),
             vars = vars,
             pollutant = "value",
             n.trees = trees,
             sam.size = nrow(data_prepared),
             n.core = n_cores)
    
    data_prepared$predicted <- predict(model$model, data_prepared, n.trees=model$model$n.trees)
    data_predict <- data_prepared %>% filter(set=="prediction") %>% filter(!is.na(value))
    model$rmse_predict <- Metrics::rmse(data_predict$value, data_predict$predicted)
    model$mae_predict <- Metrics::mae(data_predict$value, data_predict$predicted)
    model$rsquared_predict <- 1 - sum((data_predict$predicted - data_predict$value)^2) / sum((data_predict$value - mean(data_predict$value))^2)
    
    data_training <- data_prepared %>% filter(set=="training") %>% filter(!is.na(value))
    model$rmse_training <- Metrics::rmse(data_training$value, data_training$predicted)
    model$mae_training <- Metrics::mae(data_training$value, data_training$predicted)
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
      normalised <- deweather::metSim(model, data_prepared, metVars=weather_vars, n.core=n_cores, B=samples) %>%
        rename(value_predict=pred)
      res <- res %>% mutate(normalised=list(normalised))
      if(detect_breaks){
        #TODO
      }
    } 
    

    if("date_unix" %in% time_vars){
      # Save trend impact (equivalent to weather corrected?)
      # Different form of output depending on timevars. Trying both
      tryCatch({
        res$trend <- list(model$pd %>% dplyr::filter(var=="date_unix") %>%
                            dplyr::mutate(date=lubridate::decimal_date(x)) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(date, mean, lower, upper))  
      }, error=function(err){
        res$trend <- list(model$pd %>% dplyr::filter(var=="date_unix") %>%
                            dplyr::mutate(date=lubridate::date_decimal(as.numeric(x))) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(date, mean, lower, upper))  
      })
    }
    res
}