#' Database maintenance and migration functions
#'
#' One-off functions for bulk transforms, metadata backfills, and cleanup
#' of the creafire GridFS collections. These are not part of normal workflows
#' and are typically run manually from the console.
#'
#' Depends on core db functions from db.R.

#' Download all weather, apply a function, and reupload
#' @export
db.apply_function_to_weather <- function(f=db.clean_weather_function(), location_id=NULL, met_type=NULL, height=NULL, duration_hour=NULL, hours=NULL, buffer_km=NULL, fire_source=NULL, fire_split_regions=NULL){

  weathers <- db.download_weather(location_id=location_id,
                                  met_type=met_type,
                                  height=height,
                                  duration_hour=duration_hour,
                                  hours=hours,
                                  buffer_km=buffer_km,
                                  fire_source=fire_source,
                                  fire_split_regions=fire_split_regions)

  if(nrow(found)==0) return(NULL)

  weathers <- weathers %>% mutate(weather = map(weather, f))

  # Reupload
  for(i in seq(nrow(weathers))){
    print(sprintf("%d/%d",i,nrow(weathers)))
    w <- weathers[i,]
    db.upload_weather(weather=w$weather,
                      location_id=w$location_id,
                      location_name=w$location_name,
                      weather_sources=w$weather_sources,
                      met_type=w$met_type,
                      height=w$height,
                      duration_hour=w$duration_hour,
                      buffer_km=w$buffer_km,
                      hours=w$hours,
                      fire_source=w$fire_source,
                      fire_split_regions = w$fire_split_regions
    )
  }
}


#' Return a function that removes rows with NA/infinite air_temp_min
db.clean_weather_function <- function(){
  f <- function(x){x %>% filter(!is.na(air_temp_min), !is.infinite(air_temp_min))}
  return(f)
}


#' Backfill location_name in weather metadata from rcrea
db.add_location_name <- function(){

  fs <- db.get_gridfs_weather()
  col <- db.get_collection('weather.files')
  found <- fs$find('{}')

  location_ids <- lapply(found$metadata, function(x) jsonlite::fromJSON(x)$location_id) %>%
    unlist() %>%
    unique()

  lapply(location_ids, function(location_id){
    location_name <- rcrea::cities(id=location_id)$name
    col$update(
      query = sprintf('{"metadata.location_id": "%s"}',location_id),
      update = sprintf('{ "$set" : { "metadata.location_name" : "%s"} }', location_name),
      multiple=T
    )
  })
}


#' Backfill fire_split_regions field in existing records
db.add_split_regions <- function(){

  fs <- db.get_gridfs_weather()
  col <- db.get_collection('weather.files')
  col$update(
    query = sprintf('{"metadata.fire_split_regions": {}}'),
    update = sprintf('{ "$set" : { "metadata.fire_split_regions" : "%s"} }', NO_SPLIT_REGION),
    multiple=T
  )

  fs <- db.get_gridfs_meas()
  col <- db.get_collection('meas.files')

  col$update(
    query = sprintf('{"metadata.fire_split_regions": {}}'),
    update = sprintf('{ "$set" : { "metadata.fire_split_regions" : "%s"} }', NO_SPLIT_REGION),
    multiple=T
  )

}

#' Backfill weather_sources field in existing records
db.add_weather_sources <- function(){

  fs <- db.get_gridfs_weather()
  col <- db.get_collection('weather.files')
  col$update(
    # query those without metadata.weather_sources
    query = sprintf('{"metadata.weather_sources": {}}'),
    update = sprintf('{ "$set" : { "metadata.fire_split_regions" : "%s"} }', "noaa"),
    multiple=T
  )
}


#' Remove empty (zero-size) files from weather and meas collections
db.clean <- function(){
  fs <- db.get_gridfs_weather()

  found <- fs$find('{}')
  empty <- found[found$size==0,]
  if(nrow(empty)>0) fs$remove(paste0("id:", empty$id))

  fs <- db.get_gridfs_meas()

  # Remove those that don't have process_id
  found <- fs$find('{}')
  empty <- found[found$size==0,]
  if(nrow(empty)>0) fs$remove(paste0("id:", empty$id))

  # old <- found[found$date < "2023-10-01",]
  # fs$remove(paste0("id:", old$id))
}
