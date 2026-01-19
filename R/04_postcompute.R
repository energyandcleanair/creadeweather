
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

postcompute <- function(results){
  mapply(postcompute_one,
         models=results$models,
         performances=results$performances,
         data=results$data,
         config=results$config,
         location_id=results$location_id,
         poll=results$poll,
         unit=results$unit,
         source=results$source,
         process_id=results$process_id,
         USE.NAMES=F,
         SIMPLIFY=FALSE
         ) %>%
    do.call(bind_rows, .)
}



postcompute_one <- function(models, data, config, location_id, poll, unit, source, process_id, performances, ...){
  
  engine <- config$engine
  keep_models <- config$keep_models
  
  if(engine=='gbm'){
    result <- postcompute_gbm(models=models, data=data, config=config, performances=performances, ...) %>%
      mutate(location_id=location_id,
             poll=poll,
             unit=unit,
             source=source,
             process_id=process_id)
  }else{
    stop("Engine not supported")
  }
  return(result)
}