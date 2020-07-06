upload_process_meas <- function(meas_process_id, process_deweather, poll, unit, region_id, normalised, source, preferred_name=NULL){
  
  # Create or retrieve process
  meas_process <- rcrea::processes() %>% filter(id==meas_process_id) %>% dplyr::collect()
  deweather_process_id = rcrea::retrieve_or_create_process(meas_process$filter[[1]],
                                                           meas_process$agg_spatial[[1]],
                                                           meas_process$agg_temp[[1]],
                                                           deweather=process_deweather,
                                                           preferred_name=preferred_name
  )
  
  normalised_meas <- normalised %>%
    mutate(process_id=deweather_process_id,
           poll=poll,
           unit=unit,
           source=source,
           region_id=region_id
           )
  
  rcrea::upsert_meas(normalised_meas)
  return(deweather_process_id)
}