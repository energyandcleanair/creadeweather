#' Getting measurements
#'
#' @param meas 
#' @param poll 
#' @param source 
#' @param country 
#' @param location_id 
#' @param location_type 
#' @param process_id 
#' @param city 
#' @param aggregate_level 
#' @param nest_and_sf 
#'
#' @return
#' @export
#'
#' @examples
get_measurements <- function(meas=NULL,
                             poll=NULL,
                             source=NULL,
                             country=NULL,
                             date_from=NULL,
                             location_id=NULL,
                             location_type=NULL,
                             process_id=NULL,
                             city=NULL,
                             aggregate_level="city",
                             nest_and_sf=T){
  
  
  if(is.null(meas)){
    meas <- rcrea::measurements(poll=poll,
                                country=country,
                                location_id=location_id,
                                location_type=location_type,
                                city=city,
                                aggregate_level=aggregate_level,
                                date_from=date_from,
                                source=source,
                                deweathered=F,
                                process_id=process_id,
                                with_metadata=T,
                                with_geometry=T)
  }else{
    meas <- as.data.frame(meas)
  }
  
  # Sometimes, group_by with geometry doesn't work. We split in two steps
  meas_geom <- meas %>% dplyr::distinct(location_id, geometry, timezone)
  
  # For some timezone or summer/winter time related reasons (or bad aggregation?),
  # certain (very few) days in some regions have two measurements,
  # which will ultimately fail due to UNIQUE constraints in Postgres
  # We prevent this now.
  meas <- meas %>% 
    dplyr::group_by(date=lubridate::date(date), location_id, poll, unit, source, timezone, process_id, country) %>%
    dplyr::summarise(value=mean(value, na.rm=T))
  
  if(nrow(meas)==0){
    stop("No measurement found")
  }
  
  if(nest_and_sf){
    # Nest
    meas <- meas %>%
      dplyr::group_by(location_id, poll, unit, source, process_id, country) %>%
      tidyr::nest() %>%
      dplyr::rename(meas=data) %>%
      dplyr::ungroup()
    
    # Transform to sf
    meas <- meas %>%
      dplyr::ungroup() %>%
      dplyr::left_join(meas_geom, by=c("location_id")) %>%
      dplyr::mutate(geometry=suppressWarnings(sf::st_centroid(geometry))) %>%
      sf::st_as_sf(sf_column_name="geometry", crs = 4326)
  }
  
  
  return(meas)
}


