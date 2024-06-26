cfs.add_weather <- function(weather, weather_vars){
  
  vars=c("pbl_min","pbl_max")
  dir_cfs <- cfs.folder_cfs()
  dir_pbl <- cfs.folder_pbl()
    
  location_dates <- weather %>%
    tidyr::unnest(weather) %>%
    distinct(location_id, timezone, date, geometry)
  
  dates <- unique(location_dates %>% pull(date))
  cfs.refresh_files(dates)
  
  locations_sf <- st_as_sf(weather %>%
                            ungroup() %>%
                            dplyr::select(location_id, geometry) %>%
                            dplyr::distinct(location_id, .keep_all=T))

  
  # Collecting all positions to be read for each file
  # And group so we can use raster::stack
  dates_files <- location_dates %>%
    rowwise() %>%
    mutate(fp=list(cfs.date_to_filepaths(date, dir_pbl, vars)),
           date_group=strftime(date,"%Y")) %>%
    tidyr::unnest(fp) %>%
    tidyr::nest(data=-date_group)
  
  
  process_date_group <- function(data){
    
    fps <- data %>% distinct(fp, date, variable) %>%
      filter(file.exists(fp))
    
    gs <- sf::st_as_sf(data %>% distinct(location_id,geometry))
    ts <- terra::rast(fps$fp)
    pbls <- terra::extract(ts, st_coordinates(gs))
    
    
    pbls.df <- data.frame(value=t(pbls))
    names(pbls.df) <- gs$location_id
    pbls.df$variable <- fps$variable
    pbls.df$date <- fps$date
    
    pbls.tojoin <- tibble(pbls.df) %>%
      tidyr::pivot_longer(cols=-c(date, variable), names_to="location_id", values_to="value") %>%
      tidyr::pivot_wider(names_from=variable, values_from=value)
    
    data %>%
      distinct(location_id, date) %>%
      dplyr::left_join(pbls.tojoin)
  }
  
  print(paste("Extracting pbl..."))
  pbl_values <- do.call("rbind", pbapply::pblapply(dates_files$data, process_date_group))
  pbl_values <- pbl_values %>%
    select_at(c('location_id', 'date', intersect(names(.), weather_vars))) %>%
    group_by(location_id) %>%
    tidyr::nest()
  print(paste("Done"))
  
  # Join to weather data
  weather_new <- weather %>%
    left_join(pbl_values) %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(weather)) %>%
    mutate(weather=list(weather %>%
                          left_join(data %>%
                                      select(c('date', intersect(names(data), weather_vars))),
                                    by="date"))) %>%
    select(-c(data))
  
  return(weather_new)
}

cfs.folder_cfs <- function(){
  dir_cfs<- Sys.getenv("DIR_CFS")
  if(dir_cfs==""){
    stop("Missing DIR_CFS folder")
  }
  
  return(dir_cfs)
}

cfs.folder_pbl <- function(){
  # We extract pbl from original cfs and 
  # save in smaller files locally
  
  dir_pbl<- Sys.getenv("DIR_PBL")
  if(dir_pbl==""){
    stop("Missing DIR_PBL folder")
  }
  dir_pbl_cfs <- file.path(dir_pbl, "cfs")
  dir.create(dir_pbl_cfs, showWarnings = F, recursive = T)
  
  return(dir_pbl_cfs)
}

cfs.processed_dates <- function(){
  dir_pbl <- cfs.folder_pbl()
  ds <- list.files(path = dir_pbl, full.names = F, recursive = TRUE)
  dates <- unique(lubridate::ymd(ds))
  dates <- dates[!is.na(dates)]
  return(dates)
}
 
cfs.processable_dates <- function(dates){

  unavailable <- lubridate::date(
    c(
      "2015-11-10",
      "2015-11-11",
      "2015-12-15",
      "2015-12-16",
      "2017-08-15",
      "2017-08-16",
      "2019-03-19",
      "2019-03-20",
      "2019-10-07",
      "2019-10-08",
      "2019-10-09",
      "2019-10-16",
      "2019-10-17",
      "2020-01-20",
      "2020-01-21",
      "2020-01-28",
      "2020-01-29",
      "2020-02-27",
      "2020-02-28"
      ))

  dates[!dates %in% unavailable]
}
 


#' Build daily summaries (min, max. mean) of pbl height using local timezone
#' Original cfs files are read from dir_cfs
#' And result files are saved in dir_pbl
#'
#' @param date 
#' @param cache_rs 
#'
#' @return
#' @export
#'
#' @examples
cfs.process_date <- function(date, cache_rs=NULL){

  tryCatch({
    
  
    hours <- seq(lubridate::date(date) - lubridate::hours(14),
                 lubridate::date(date) + lubridate::hours(24+11),
                 by="h")
    
    tz_sf <- cfs.timezone_sf()
    dir_cfs <- cfs.folder_cfs()
    dir_pbl <- cfs.folder_pbl()
    
    
    # Read rasters or retrieve from cache
    rs <- lapply(hours,
                 function(h, cache_rs, dir_cfs){
                   if(!is.null(cache_rs) && (as.character(h) %in% names(cache_rs))){
                     # Use cache from previouday processing if can
                     rh <- cache_rs[[as.character(h)]]
                   }else{
                     fp <- cfs.hour_to_filepath(h, dir_cfs)
                     rh <- tryCatch({
                       raster::raster(fp, varname="HPBL_surface") %>%
                         # Moving from 0-360 to -180-180
                         # To match locations and timezone shapefiles
                         raster::rotate()
                     }, error=function(x){
                       print(sprintf("Trying trick with cfs %s",fp))
                       return(tryCatch({
                         h_safe <- as.Date("2021-01-01")
                         fp_safe <- cfs.hour_to_filepath(h_safe, dir_cfs)
                         dim_safe <- stars::read_ncdf(fp_safe, var="HPBL_surface") %>% stars::st_dimensions()
                         stars::read_ncdf(fp, var="HPBL_surface") %>%
                                stars::`st_dimensions<-`(dim_safe) %>%
                                as("SpatRaster") %>%
                                raster::raster() %>%
                                raster::rotate()
                       }, error=function(e){
                         print("No data could be extracted")
                         return(NULL)
                       }))
                     })
                   }
                   rh
                 },
                 cache_rs=cache_rs,
                 dir_cfs=dir_cfs)
    
    names(rs) <- as.character(hours)
    valid <-!unlist(lapply(rs, is.null))
    rs <- rs[valid]
    hours <- hours[valid]
    
    # Mask rasters based on date
    rs.masked <- mapply(
                 function(rh, h, date, tz_sf){
  
                   tz_sf_mask <- tz_sf %>% dplyr::filter(
                     lubridate::date(!!h + lubridate::hours(floor(zone)))==lubridate::date(date))
                   # Below filter is ideal cause it'd consider summer/winter,
                   # but many timezones names are missing or not recognized
                   # lubridate::date(date)==
                   # lubridate::date(lubridate::with_tz(hour, tzone=tz_name1st)))
  
                   return(raster::mask(
                     rh,
                     tz_sf_mask))
  
                 },
                 rh=rs,
                 h=hours,
                 date=date,
                 tz_sf=list(tz_sf))
    
    rs.stacked <- raster::brick(rs.masked)
    rs.min = raster::calc(rs.stacked, min)
    rs.max = raster::calc(rs.stacked, max)
    rs.mean = raster::calc(rs.stacked, mean)
    
    raster::writeRaster(rs.min, file.path(dir_pbl, paste0(gsub("-","",date),"_min.nc")))
    raster::writeRaster(rs.max, file.path(dir_pbl, paste0(gsub("-","",date),"_max.nc")))
    raster::writeRaster(rs.mean, file.path(dir_pbl, paste0(gsub("-","",date),"_mean.nc")))
  
    return(rs)
  },
  error=function(c){
    warning(paste("Failed for date", date))
    return(NULL)
  })
}

cfs.timezone_sf <- function(){
  
  folder <- file.path(Sys.getenv("DIR_DATA"),"boundaries","timezones","naturalearth") 
  fp <- file.path(folder, "ne_10m_time_zones.shp")
  
  if(!file.exists(fp)){
    dir.create(folder, recursive = T, showWarnings = F)
    fp_zip <- gsub("\\.shp","\\.zip", fp)
    download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_time_zones.zip",
                  fp_zip)
    unzip(fp_zip, exdir=folder)
  }
  
  sf::read_sf(fp)
}

cfs.refresh_files <- function(dates){

  processed <- cfs.processed_dates()
  processable <- cfs.processable_dates(dates)
  to_process <-  processable[!processable %in% processed]

  cache_rs=NULL
  for(i in seq_along(to_process)){
    d <- to_process[i]
    print(paste("Processing", d))
    cache_rs=cfs.process_date(d, cache_rs)
  }
}


cfs.day_to_utc_hours <- function(date, timezone){
  hour_start <- lubridate::floor_date(
    lubridate::with_tz(
      lubridate::force_tz(date, tz=timezone),
      tz="UTC"),
    unit="h")
  hour_end <- hour_start + lubridate::hours(23)
  
  seq(hour_start, hour_end, by="h")
}

cfs.hour_to_filepath <- function(h, dir_cfs){
  file.path(dir_cfs,
            strftime(h, "%Y"),
            strftime(h, "%m"),
            strftime(h, "%d"),
            paste0(
              "cfs_",
              strftime(h, "%Y%m%d_%H%M%S"),
              ".nc"))
}

cfs.date_to_filepaths <- function(d, dir_pbl, vars=c("pbl_min","pbl_max","pbl_mean")){
  tibble(variable=vars,
         fp=file.path(dir_pbl,
            paste0(gsub("-","",d),gsub("pbl","",vars),".nc")))
}



