# Attaching fire information
# 2 x 2  options
# Using active fires points vs fire radiative power raster
# Using a buffer around geometry vs a buffer around trajectories (more time consuming)


#' Add fire related information to weather data
#'
#' @param weather the dataframe to add fire information to
#' @param source should we use FIRMS active fire data or GFAS emission data (either "gfas" or "viirs")
#' @param mode should we consider a simple buffer around geometry, a wind oriented buffer, or calculate trajectories.
#' attach_mode can be "circular", "oriented" or "trajectory"
#' @param buffer_km buffer around geometry or trajectories. Default 200km for `circular` and `oriented`, 10km for `trajectory`
#' @param met_type met_type used for hysplit (if mode=="trajectory" or mode=="dispersion")
#' @param height height used for hysplit (if mode=="trajectory")
#' @param duration_hour duration in hours used for hysplit (if mode=="trajectory" or mode=="dispersion")
#' @param trajs_height receptor height for trajectories in meter.
#' If null or NA, pbl average will be considered.
#' @param upload_weather upload weather data to our MongoDB to be used in the biomass burning dashboard
#'
#' @return
#' @export
#'
#' @examples
fire.add_fire <- function(weather,
                          weather_sources=NULL, # used for cache only
                          source="viirs", #or "gfas"
                          mode="trajectory",
                          met_type="gdas1",
                          duration_hour=120,
                          delay_hour=24,
                          buffer_km=NULL,
                          split_days=F,
                          split_regions=NULL,
                          split_regions_res='low',
                          trajs_height=NULL,
                          trajs_parallel=T,
                          trajs_cores=parallel::detectCores() - 1,
                          trajs_height_default=10,
                          trajs_hours=seq(0,23,4),# To make it faster, we don't calculate trajectories every hour
                          use_trajs_cache=T,
                          use_weather_cache=T,
                          upload_trajs=F,
                          save_trajs_filename=NULL,
                          upload_weather=F){
  
  
  if(is.null(buffer_km)){
    buffer_km <- switch(mode,"circular"=200, "oriented"=200, "trajectory"=10)
  }
  
  
 
  if(use_weather_cache){
    # Add existing fire data (that is stored in MongoDB)
    weather <- fire.add_existing_weather(weather=weather,
                                         source=source,
                                         mode=mode,
                                         met_type=met_type,
                                         duration_hour=duration_hour,
                                         delay_hour=delay_hour,
                                         buffer_km=buffer_km,
                                         split_days=split_days,
                                         split_regions=split_regions,
                                         trajs_height=trajs_height,
                                         trajs_hours=trajs_hours)
  }
  
  
  loc_dates <- fire.get_loc_dates_to_update(weather = weather,
                                         trajs_height = trajs_height,
                                         trajs_height_default = trajs_height_default)
  
  
  
  print(sprintf("Found %d weather rows. Need to update fire data for %d of them",
               nrow(tibble(weather) %>%
                      tidyr::unnest(weather)),
               nrow(loc_dates)))
  
  # If no need to add fire, move on
  if(nrow(loc_dates) == 0) return(weather)
  
  loc_dates_fire <- fire.get_fire_at_loc_dates(
    loc_dates = loc_dates,
    mode = mode,
    met_type = met_type,
    duration_hour = duration_hour,
    trajs_hours = trajs_hours,
    upload_trajs = upload_trajs,
    use_trajs_cache = use_trajs_cache,
    trajs_parallel = trajs_parallel,
    trajs_cores = trajs_cores,
    buffer_km = buffer_km,
    split_days = split_days,
    split_regions = split_regions,
    split_regions_res = split_regions_res,
    save_trajs_filename = save_trajs_filename,
    source=source
  )
 
  
  weather_with_fire <- fire.merge_fire_and_weather(
    weather = weather,
    loc_dates_fire = loc_dates_fire,
    fire_vars_pattern = fire.get_fire_vars_pattern(source)
  )
  
  
  if(upload_weather & mode=='trajectory'){
    print("Uploading weather")
    uploaded <- pbmapply(creafire::db.upload_weather,
                      weather=weather_with_fire$weather,
                      location_id=weather_with_fire$location_id,
                      met_type=met_type,
                      duration_hour=duration_hour,
                      buffer_km=buffer_km,
                      height=list(trajs_height),
                      fire_source=source,
                      hours=list(trajs_hours),
                      fire_split_regions=ifelse(is.null(split_regions), creafire::NO_SPLIT_REGION, split_regions),
                      SIMPLIFY=F
                      )
    
    if(length(uploaded) != nrow(weather_with_fire)){
      warning("Probably failed to upload weather")
    }
  }
  
  return(weather_with_fire)
}




fire.get_fire_at_loc_dates <- function(loc_dates,
                                   mode,
                                   met_type,
                                   duration_hour,
                                   trajs_hours,
                                   use_trajs_cache,
                                   upload_trajs,
                                   trajs_parallel,
                                   trajs_cores,
                                   buffer_km,
                                   split_days,
                                   split_regions,
                                   split_regions_res,
                                   save_trajs_filename,
                                   source
                                   ){
  
  if(nrow(loc_dates) == 0) return(tibble())
  
  attach_fn <- fire.get_attach_to_trajs_fn(source=source)
  
  if(mode=="trajectory"){
    
    if(!"geometry" %in% names(loc_dates)){
      locations <- rcrea::locations(id=unique(loc_dates$location_id), with_source = F)
      loc_dates <- loc_dates %>%
        left_join(locations %>% select(location_id=id, geometry))
    }
    
    print(sprintf("Calculating trajs for %d dates", length(loc_dates$date)))
    loc_dates$trajs <- creatrajs::trajs.get(
      dates=loc_dates$date,
      location_id=loc_dates$location_id,
      geometry=loc_dates$geometry,
      met_type=met_type,
      height=loc_dates$trajs_height,
      duration_hour=duration_hour,
      hours=trajs_hours, 
      timezone=unique(loc_dates$timezone),
      use_cache=use_trajs_cache,
      parallel=trajs_parallel,
      mc.cores = trajs_cores
    )
    names(loc_dates$trajs) <- NULL
    print("Done")
    
    if(!is.null(save_trajs_filename)){
      print(paste("Exporting trajectories to ", save_trajs_filename))
      saveRDS(loc_dates, save_trajs_filename)
      print("Done")
    }
    
    if(upload_trajs){
      print("Uploading trajs")
      pbapply::pbmapply(creatrajs::db.upload_trajs,
                        trajs=loc_dates$trajs,
                        location_id=loc_dates$location_id,
                        hours=list(trajs_hours),
                        met_type=met_type,
                        duration_hour=duration_hour,
                        height=loc_dates$trajs_height,
                        date=loc_dates$date)
    }
    
    print(sprintf("Attaching fires to trajectories for %d dates", length(loc_dates$date)))
    loc_dates_fire <- tryCatch({
      attach_fn(loc_dates,
                buffer_km=buffer_km,
                parallel=trajs_parallel,
                mc.cores=max(round(parallel::detectCores()-2), 1),
                split_days=split_days,
                split_regions=split_regions,
                adm_res=split_regions_res)
    }, error=function(e){
      print(sprintf("Failed to attach fires. Adding NAs instead, Error: %s", e))
      return(loc_dates %>%
               rowwise() %>%
               mutate(fires=list(tibble(fire_frp=NA, fire_count=NA))) %>%
               ungroup())
    })
    print("Done")
    
  }else if(mode=="oriented"){
    if(source=="gfas"){
      stop("GFAS not yet supported with oriented extent")
    }
    print("Calculating extents")
    loc_dates <- loc_dates %>%
      rowwise() %>%
      rename(date_meas=date) %>%
      mutate(extent=creatrajs::trajs.oriented_extent(geometry,
                                                     duration_hour=duration_hour,
                                                     ws=ws_copy,
                                                     wd=wd_copy))
    print("Done")
    
    print("Attaching fires to extents")
    loc_dates_fire <- creatrajs::fire.attach_to_extents(loc_dates, delay_hour=duration_hour)
    print("Done")
    
  }else if(mode=="circular"){
    print("Calculating extents")
    if(source=="gfas"){
      stop("GFAS not yet supported with circular extent")
    }
    
    loc_dates <- loc_dates %>%
      rowwise() %>%
      rename(date_meas=date) %>%
      mutate(extent=creatrajs::trajs.circular_extent(geometry,
                                                     buffer_km=buffer_km))
    print("Done")
    
    print("Attaching fires to extents")
    loc_dates_fire <- creatrajs::fire.attach_to_extents(loc_dates, delay_hour=duration_hour)
    print("Done")
  }
  
  return(loc_dates_fire)
}


#' Return location, dates for which we need fire data
#' Also adds information required such as wind direction and height
#'
#' @param weather 
#' @param mode 
#'
#' @return
#' @export
#'
#' @examples
fire.get_loc_dates_to_update <- function(weather, trajs_height, trajs_height_default){
  
  cols <- setdiff(c(names(weather), "date", "trajs_height", "wd_copy", "ws_copy"), "weather")
  
  tibble(weather) %>%
    tidyr::unnest(weather) %>%
    filter(if('fire_frp' %in% names(.)) is.na(fire_frp) else T) %>%
    # We keep wind information (in case mode=='oriented')
    # as well as pbl for trajectory height (in case mode=='trajectory', used for height in splitr)
    rowwise() %>%
    mutate(wd_copy=wd,
           ws_copy=ws,
           trajs_height=ifelse(is.null(!!trajs_height) || is.na(!!trajs_height),
                               ifelse(all(c("pbl_min","pbl_max") %in% names(.)),
                                      (pbl_min + pbl_max)/2,
                                      trajs_height_default),
                               !!trajs_height)) %>%
    mutate(trajs_height=tidyr::replace_na(trajs_height, trajs_height_default)) %>%
    tidyr::nest(weather=!cols)
  
}


#' Look on MongoDB for what matching fire data we already have
#'
#' @return
#' @export
#'
#' @examples
fire.add_existing_weather <- function(weather,
                                      source,
                                      mode,
                                      met_type,
                                      duration_hour,
                                      delay_hour,
                                      buffer_km,
                                      split_days,
                                      split_regions,
                                      trajs_height,
                                      trajs_hours){
  
  if(mode != 'trajectory'){return(weather)}
  
  split_regions <- ifelse(is.null(split_regions), creafire::NO_SPLIT_REGION, split_regions)
  location_ids <- unique(weather$location_id)
  
  available_weathers <- lapply(location_ids, function(location_id){
    creafire::db.download_weather(location_id=location_id,
                                  duration_hour=duration_hour,
                                  height=trajs_height,
                                  met_type=met_type,
                                  buffer_km=buffer_km,
                                  fire_source=source,
                                  fire_split_regions=split_regions,
                                  hours=paste0(trajs_hours, collapse=","))  
  }) %>%
    do.call(bind_rows, .)
  
  if(nrow(available_weathers) > 0){
    weather %>%
      left_join(available_weathers %>%
                  select(location_id, weather_fire=weather)) %>%
      rowwise() %>%
      mutate(weather=list(left_join(weather, weather_fire %>% select_at(grep('date|fire', names(.), value=T))))) %>%
      dplyr::select(-c(weather_fire)) %>%
      ungroup()
  }else{
    weather
  }
}


# Adding new fire data to weather
fire.merge_fire_and_weather <- function(weather, loc_dates_fire, fire_vars_pattern){
  
  if(nrow(loc_dates_fire) == 0) return(weather)
  
  loc_dates_fire_long <- loc_dates_fire %>%
    unnest(fires) %>%
    select_at(c("location_id", "date",
                grep(fire_vars_pattern, names(.), value=T)))
    
  fire_vars <- grep(fire_vars_pattern, names(loc_dates_fire_long), value=T)
  
  results <- weather %>%
    left_join(loc_dates_fire_long %>%
                ungroup() %>%
                nest(fires=-c(location_id))) %>%
    rowwise() %>%
    mutate(weather=list(creahelpers::fill_df1_with_df2(weather,
                                                       fires,
                                                       join_cols=c("date"),
                                                       value_cols=fire_vars
                                                       ))) %>%
    dplyr::select(-c(fires)) %>%
    ungroup()
}


fire.get_fire_vars_pattern <- function(source){
  if(source=="gfas"){
    return("pm25_emission")
  }else if(source=="viirs"){
    attach_fn <- creatrajs::fire.attach_to_trajs
    return("^fire_.*")
  }
  return(NULL)
}

fire.get_attach_to_trajs_fn <- function(source){
  
  if(source=="gfas"){
    return(creatrajs::gfas.attach_to_trajs)
  }else if(source=="viirs"){
    return(creatrajs::fire.attach_to_trajs)
  }
  return(NULL)
}

fire.split_archive <- function(file_archive, region="Global"){
  
  f <- readr::read_csv(file_archive)
  fs <- split(f, f$acq_date)
  
  folder <- file.path(creatrajs::utils.get_firms_folder(),
                      "suomi-npp-viirs-c2",
                      region)
  
  lapply(fs, function(f){
    date <- unique(f$acq_date)
    file_day <- file.path(folder,
                          sprintf("fire_archive_global_%s.txt",
                                  strftime(date, "%Y%j")))
    
    # Remove existing file at that date
    existing_files <- list.files(path=folder,
                                 pattern=sprintf("%s.[txt|csv]", strftime(date, "%Y%j")),
                                 full.names = T)
    lapply(existing_files, file.remove)
    
    # Export
    readr::write_csv(f, file_day)
  })
  
  file.remove(file_archive)
}
#' 
#' #' Archive active fire files were downloaded one year by one year
#' #' which creates memory issues. We split them in more digestible ones.
#' #' Should only be run once, manually on downloaded files.
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' frp.split_yearly_activefire_by_date <- function(f){
#' 
#'   d <- data.table::fread(f)
#'   dates <- unique(d$acq_date)
#' 
#'   write_date <- function(date, d, f){
#'     f_date <- file.path(dirname(f),
#'                         gsub("[0-9]{4}", strftime(date, "%Y%j"),
#'                              gsub("csv", "txt", basename(f))))
#'     write.csv(d[d$acq_date==date], f_date, row.names=FALSE)
#'   }
#'   pbapply::pblapply(dates, write_date, d=d, f=f)
#' }