#' Post compute results
#'
#' @param engine 
#' @param result_folder 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
post_compute <- function(engine, result_folder, ...){
  
  if(!engine %in% c('gbm', 'rmweather')){
    stop("'engine' should be either 'gbm' or 'rmweather'")
  }
  
  switch(engine,
         # "gbm"=post_compute_gbm(meas_weather=meas_weather,
         #                        pollutants=pollutants,
         #                        exp_suffix=exp_suffix,
         #                        ...),
         "rmweather"=postcompute_results_rmweather(result_folder=result_folder,
                                            ...)
  )
}
