#' Title
#'
#' @param engine 
#' @param meas_weather 
#' @param pollutants 
#' @param exp_suffix 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
train_models <- function(engine, meas_weather, pollutants, exp_suffix=NULL, ...){
  
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
  