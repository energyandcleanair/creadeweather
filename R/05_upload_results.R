upload_results <- function(results, deweather_process_id){
  
  processes <- rcrea::processes()
  # processes <- results %>% distinct(process_id, process_deweather)
  
  mapply(upload_results_one,
         config=results$config,
         result=results$result,
         process_id=results$process_id,
         processes=list(processes),
         poll=results$poll,
         unit=results$unit,
         location_id=results$location_id,
         source=results$source,
         deweather_process_id=deweather_process_id,
         USE.NAMES=F,
         SIMPLIFY=FALSE
         ) %>%
    do.call(bind_rows, .)
}


upload_fire_results <- function(results,
                                met_type,
                                duration_hour,
                                fire_source,
                                fire_split_regions,
                                fire_buffer_km,
                                trajs_height,
                                trajs_hours
                                ){
  
  results_list <- results %>%
    group_by(location_id) %>%
    tidyr::nest()
  
  mapply(creafire::db.upload_meas,
         meas=results_list$data,
         location_id=results_list$location_id,
         met_type=met_type,
         duration_hour=duration_hour,
         hours=list(trajs_hours),
         buffer_km=fire_buffer_km,
         height=list(trajs_height),
         fire_source=fire_source,
         fire_split_regions=list(fire_split_regions))
}


upload_results_one <- function(config, result, process_id, processes, poll, unit, location_id, source, deweather_process_id){
  
  if(is.null(deweather_process_id)){
    deweather_process_id <- config_to_process_deweather(config=config,
                                                        process_id=process_id,
                                                        processes=processes)
  }
  
  message(sprintf('Uploading with process_id %s', deweather_process_id))
  upload_meas(result, deweather_process_id, poll, unit, location_id, source)
}


config_to_process_deweather <- function(config, process_id, processes){
  
  process_deweather_dict <- gsub("'","\"",paste0("{",
                       "'engine':'",config$engine,"',",
                       "'trees':'",config$trees,"',",
                       "'learning.rate':'",config$learning.rate,"',",
                       "'interaction.depth':'",config$interaction.depth,"',",
                       "'training.fraction':'",config$training.fraction,"',",
                       "'lag':'",config$lag,"',",
                       "'training_start':'",config$training_start,"',",
                       "'training_end':'",config$training_end,"',",
                       "'time_vars':['",paste0(sort(config$time_vars),collapse="','"),"'],",
                       "'weather_vars':['",paste0(sort(config$weather_vars),collapse="','"),"'],",
                       "'link':'",config$link,"'",
                       "}")
  )
  
  
  preferred_name <- config_to_preferred_name(config=config,
                                             process_id=process_id,
                                             processes=processes)
  
  meas_process <- processes %>% filter(id==process_id)
  
  deweather_process_id = rcrea::retrieve_or_create_process(
    filter=meas_process$filter[[1]],
    agg_spatial=meas_process$agg_spatial[[1]],
    agg_temp=meas_process$agg_temp[[1]],
    deweather=process_deweather_dict,
    preferred_name=preferred_name
  )
  
  return(deweather_process_id)
}


config_to_preferred_name <- function(config, process_id, processes){
  
  filter_type <- processes %>%
      filter(id == !!process_id) %>%
      pull(filter_type)
  
  paste0(
    config$output,
    "_",config$engine,
    "_lag",config$lag,
    "_",filter_type,
    ifelse(any(grepl('pbl', config$weather_vars)), "", "_nopbl"),
    ifelse(any(grepl('fire', config$weather_vars)), "_fire", ""))
}


upload_meas <- function(result, deweather_process_id, poll, unit, location_id, source){
  to_upload <- result %>%
    mutate(process_id=deweather_process_id,
           poll=!!poll,
           unit=!!unit,
           source=!!source,
           location_id=!!location_id
           )
  
  rcrea::upsert_meas(to_upload)
  return(to_upload)
}