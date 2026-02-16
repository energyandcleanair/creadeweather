#' Database functions for fire-weather and measurement data caching
#'
#' Core CRUD operations for weather and measurement data stored in MongoDB (GridFS).
#' Database: "creafire", Collections: "weather" and "meas" (GridFS prefixes).
#' Requires CREA_MONGODB_URL environment variable (typically set in .Renviron).
#'
#' See db_maintenance.R for one-off migration and cleanup functions.

#' Default value for fire_split_regions when no region splitting is used
#' @export
NO_SPLIT_REGION = "none"

#' Get a MongoDB collection connection
db.get_collection <- function(collection_name){
  readRenviron(".Renviron")
  connection_string=Sys.getenv("CREA_MONGODB_URL")
  mongolite::mongo(collection=collection_name, db="creafire", url=connection_string)
}

#' Get GridFS connection for weather data
db.get_gridfs_weather <- function(){
  readRenviron(".Renviron")
  connection_string=Sys.getenv("CREA_MONGODB_URL")
  mongolite::gridfs(db="creafire", prefix="weather", url=connection_string)
}

#' Get GridFS connection for measurement data
db.get_gridfs_meas <- function(){
  readRenviron(".Renviron")
  connection_string=Sys.getenv("CREA_MONGODB_URL")
  mongolite::gridfs(db="creafire", prefix="meas", url=connection_string)
}


#' Get unique index columns for weather GridFS metadata
db.get_unique_columns_weather <- function(){
  c("location_id", "duration_hour", "hours", "height", "met_type", "buffer_km", "fire_source", "fire_split_regions")
}


#' Get unique index columns for measurement GridFS metadata
db.get_unique_columns_meas <- function(){
  c("location_id", "duration_hour", "hours", "height", "met_type", "buffer_km", "fire_source", "fire_split_regions")
}

#' Extract metadata from all files in a GridFS collection
db.available_metadata <- function(fs, col_names){
  found <- fs$find()
  if(nrow(found)==0){
    # Return empty tibble
    tbl <- as_tibble(data.frame(matrix(nrow=0,ncol=length(col_names))))
    names(tbl) <- col_names
    return(tbl)
  }

  lapply(found$metadata, function(x){
    l <- jsonlite::fromJSON(x)
    # Replace empty list with NA
    l <- lapply(l, function(x){if(length(x)==0){NA}else{x}})
    as.data.frame(l)
  }) %>%
    do.call(bind_rows, .)
}

#' List all available cached weather metadata
db.available_weather <- function(){
  db.available_metadata(fs=db.get_gridfs_weather(),
                        col_names=db.get_unique_columns_weather())
}


#' List all available cached measurement metadata
db.available_meas <- function(){
  db.available_metadata(fs=db.get_gridfs_meas(),
                        col_names=db.get_unique_columns_meas())
}


#' Create a MongoDB index on a collection
db.create_index <- function(collection_name, columns, index_name, unique=T){
  cmd <- sprintf('{"createIndexes":"%s",
        "indexes":[{"key":{%s},
        "name":"%s","unique": %s}]}',
                 collection_name,
                 paste(sprintf("\"%s\":1",columns), collapse=","),
                 index_name,
                 ifelse(unique, "true","false"))

  m <- db.get_collection(collection_name)
  m$run(cmd)
}


#' Set up unique indexes for weather and measurement collections
db.setup_db <- function(){
  db.create_index(collection_name="weather.files",
                     columns=paste0("metadata.", db.get_unique_columns_weather()),
                     index_name="weather_unique_index",
                     unique=T)

  db.create_index(collection_name="meas.files",
                  columns=paste0("metadata.", db.get_unique_columns_meas()),
                  index_name="meas_unique_index",
                  unique=T)
}


#' Upload weather data to GridFS, replacing existing entry if present
#' @export
db.upload_weather <- function(weather,
                              location_id,
                              location_name=NULL,
                              weather_sources=NULL,
                              met_type,
                              height,
                              duration_hour,
                              buffer_km,
                              hours,
                              fire_source,
                              fire_split_regions=NO_SPLIT_REGION){

  fs <- db.get_gridfs_weather()
  tmpdir <- tempdir()
  filepath <- file.path(tmpdir, "weather.RDS")
  saveRDS(weather, filepath)

  format_vector <- function(x){
    if(all(is.null(x)) || all(is.na(x))) NULL else {paste0(x, collapse=',')}
  }
  hours <- format_vector(hours)
  weather_sources <- format_vector(weather_sources)
  height <- if(is.null(height) || is.na(height)) NULL else {height}
  location_name <- if(is.null(location_name)){
    rcrea::cities(id=location_id) %>% pull(name) %>% head(1)
  }else{location_name}

  metadata <- list(location_id=location_id,
                   location_name=location_name,
                   duration_hour=duration_hour,
                   weather_sources=weather_sources,
                   hours=hours,
                   height=height,
                   met_type=met_type,
                   buffer_km=buffer_km,
                   fire_source=fire_source,
                   fire_split_regions=fire_split_regions)

  # Remove first if exists
  filter <- metadata[db.get_unique_columns_weather()]
  names(filter) <- paste0("metadata.", names(filter))
  found <- fs$find(jsonlite::toJSON(filter,auto_unbox=T))
  if(nrow(found)>0){
    print("Weather already exist. Replacing it")
    fs$remove(paste0("id:", found$id))
  }

  # And then upload
  fs$upload(filepath, name=basename(filepath), content_type=NULL,
            metadata=jsonlite::toJSON(metadata, auto_unbox=T))
}


#' Upload measurement data to GridFS, replacing existing entry if present
#' @export
db.upload_meas <- function(meas, location_id, met_type, height, duration_hour, hours, buffer_km, fire_source, fire_split_regions=NO_SPLIT_REGION){
  fs <- db.get_gridfs_meas()
  tmpdir <- tempdir()
  filepath <- file.path(tmpdir, "meas.RDS")
  saveRDS(meas, filepath)

  hours <- if(all(is.null(hours)) || all(is.na(hours))) NULL else {paste0(hours, collapse=',')}
  height <- if(is.null(height) || is.na(height)) NULL else {height}

  metadata <- list(location_id=location_id,
                   duration_hour=duration_hour,
                   hours=hours,
                   height=height,
                   met_type=met_type,
                   buffer_km=buffer_km,
                   fire_source=fire_source,
                   fire_split_regions=fire_split_regions)

  # Remove first if exists
  filter <- metadata[db.get_unique_columns_meas()]
  names(filter) <- paste0("metadata.", names(filter))
  found <- fs$find(jsonlite::toJSON(filter,auto_unbox=T))
  if(nrow(found)>0){
    print("Meas already exist. Replacing it")
    fs$remove(paste0("id:", found$id))
  }

  # And then upload
  fs$upload(filepath, name=basename(filepath), content_type=NULL,
            metadata=jsonlite::toJSON(metadata, auto_unbox=T))
}


#' Query weather GridFS by metadata filters
db.find_weather <- function(location_id,
                            met_type=NULL,
                            height=NULL,
                            duration_hour=NULL,
                            hours=NULL,
                            buffer_km=NULL,
                            fire_source=NULL,
                            fire_split_regions=NULL,
                            weather_sources=NULL){
  fs <- db.get_gridfs_weather()

  format_vector <- function(x){
    if(all(is.null(x)) || all(is.na(x))) NULL else {paste0(x, collapse=',')}
  }
  hours <- format_vector(hours)
  weather_sources <- format_vector(weather_sources)
  height <- if(is.null(height) || is.na(height)) NULL else {height}
  fire_split_regions <- if(is.null(fire_split_regions) || is.na(fire_split_regions)) NO_SPLIT_REGION else {fire_split_regions}

  filter <- list(metadata.location_id=location_id,
                 metadata.duration_hour=duration_hour,
                 metadata.hours=hours,
                 metadata.weather_sources=weather_sources,
                 metadata.height=height,
                 metadata.met_type=met_type,
                 metadata.buffer_km=buffer_km,
                 metadata.fire_source=fire_source,
                 metadata.fire_split_regions=fire_split_regions)

  filter <- filter[!unlist(lapply(filter, is.null))]
  fs$find(jsonlite::toJSON(filter,auto_unbox=T))
}


#' Query measurement GridFS by metadata filters
db.find_meas <- function(location_id,
                         met_type=NULL,
                         height=NULL,
                         duration_hour=NULL,
                         hours=NULL,
                         buffer_km=NULL,
                         fire_source=NULL,
                         fire_split_regions=NULL){
  fs <- db.get_gridfs_meas()

  hours <- if(all(is.null(hours)) || all(is.na(hours))) NULL else {paste0(hours, collapse=',')}
  height <- if(is.null(height) || is.na(height)) NULL else {height}

  filter <- list(metadata.location_id=location_id,
                 metadata.duration_hour=duration_hour,
                 metadata.hours=hours,
                 metadata.height=height,
                 metadata.met_type=met_type,
                 metadata.buffer_km=buffer_km,
                 metadata.fire_source=fire_source,
                 metadata.fire_split_regions=fire_split_regions)

  filter <- filter[!unlist(lapply(filter, is.null))]
  fs$find(jsonlite::toJSON(filter,auto_unbox=T))
}


#' Remove matching measurement files from GridFS
db.remove_meas <- function(location_id=NULL, met_type=NULL, height=NULL, duration_hour=NULL, hours=NULL, buffer_km=NULL, fire_source=NULL, fire_split_regions=NULL){
  fs <- db.get_gridfs_meas()
  found <- db.find_meas(location_id=location_id, met_type=met_type, height=height, duration_hour=duration_hour, hours=hours,
                           fire_source=fire_source, buffer_km=buffer_km, fire_split_regions=fire_split_regions)

  if(nrow(found)>0) fs$remove(paste0("id:", found$id))
  print(sprintf("%d row(s) removed", nrow(found)))
}


#' Remove matching weather files from GridFS
db.remove_weather <- function(location_id, met_type=NULL, height=NULL, duration_hour=NULL, hours=NULL, buffer_km=NULL, fire_source=NULL, fire_split_regions=NULL){
  fs <- db.get_gridfs_weather()
  found <- db.find_weather(location_id=location_id,
                           met_type=met_type,
                           height=height,
                           duration_hour=duration_hour,
                           hours=hours,
                           fire_source=fire_source,
                           buffer_km=buffer_km,
                           fire_split_regions=fire_split_regions)

  if(nrow(found)>0) fs$remove(paste0("id:", found$id))
  print(sprintf("%d row(s) removed", nrow(found)))
}


#' Download weather data from GridFS with optional metadata filtering
#' @export
db.download_weather <- function(location_id=NULL, met_type=NULL, height=NULL, duration_hour=NULL, hours=NULL, buffer_km=NULL, fire_source=NULL, fire_split_regions=NULL){
  fs <- db.get_gridfs_weather()
  found <- db.find_weather(location_id=location_id, met_type=met_type, height=height, duration_hour=duration_hour, hours=hours,
                         fire_source=fire_source, buffer_km=buffer_km, fire_split_regions=fire_split_regions)

  if(nrow(found)==0) return(NULL)

  result <- lapply(found$metadata, function(x){
    l <- jsonlite::fromJSON(x)
    # Replace empty list with NA
    l <- lapply(l, function(x){if(length(x)==0){NA}else{x}})
    as.data.frame(l)
    }) %>%
    do.call(bind_rows, .)

  ids <- paste0("id:",found$id)
  weather <- lapply(ids, function(id){
    filepath <- tempfile()
    fs$download(id, filepath)
    weather <- readRDS(filepath)
    file.remove(filepath)
    return(weather)
  })

  result$weather <- weather
  tibble(result)
}


#' Download measurement data from GridFS with optional metadata filtering
#' @export
db.download_meas <- function(location_id=NULL, met_type=NULL, height=NULL, duration_hour=NULL, hours=NULL, buffer_km=NULL, fire_source=NULL, fire_split_regions=NULL){
  fs <- db.get_gridfs_meas()
  found <- db.find_meas(location_id=location_id, met_type=met_type, height=height, duration_hour=duration_hour, hours=hours,
                           fire_source=fire_source, buffer_km=buffer_km, fire_split_regions=fire_split_regions)

  if(nrow(found)==0) return(NULL)

  result <- lapply(found$metadata, function(x){
    l <- jsonlite::fromJSON(x)
    # Replace empty list with NA
    l <- lapply(l, function(x){if(length(x)==0){NA}else{x}})
    as.data.frame(l)
  }) %>%
    do.call(bind_rows, .)

  ids <- paste0("id:",found$id)
  meas <- lapply(ids, function(id){
    filepath <- tempfile()
    fs$download(id, filepath)
    meas <- readRDS(filepath)
    file.remove(filepath)
    return(meas)
  })

  result$meas <- meas
  tibble(result)
}
