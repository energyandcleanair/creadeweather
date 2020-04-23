# This file defines the models training functions
# ...which are actually defined in their own files
# Author: Hubert Thieriot


train_models_rmweather <- function(engine, meas_weather, pollutants, exp_suffix=NULL, ...){
  
  if(!engine %in% c('gbm', 'rmweather')){
    stop("'engine' should be either 'gbm' or 'rmweather'")
  }
  
  
  switch(engine,
         "gbm"=train_models_gbm(meas_weather=meas_weather,
                                pollutants=pollutants,
                                exp_suffix=exp_suffix,
                                ...),
         "rmweather"=train_models_rmweather(meas_weather=meas_weather,
                                            pollutants=pollutants,
                                            exp_suffix=exp_suffix,
                                            ...)
  )
}
  