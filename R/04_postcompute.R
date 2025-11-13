
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
         model=results$model,
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



postcompute_one <- function(model, data, config, location_id, poll, unit, source, process_id, ...){
  
  engine <- config$engine
  keep_model <- config$keep_model
  
  if(engine=='gbm'){
    result <- postcompute_gbm(model=model, data=data, config=config, ...) %>%
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