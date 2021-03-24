
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
                     rh <- raster::raster(fp, varname="HPBL_surface") %>%
                       # Moving from 0-360 to -180-180
                       # To match locations and timezone shapefiles
                       raster::rotate()
                   }
                   
                   rh
                 },
                 cache_rs=cache_rs,
                 dir_cfs=dir_cfs)
    
    names(rs) <- as.character(hours)
    
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
  
  fp <- file.path(Sys.getenv("DIR_DATA"),"boundaries","timezones","naturalearth",
                  "ne_10m_time_zones.shp")
  
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

 
# ncar.download_date <- function(date){
#   dir_ncar <- ncar.folder()
#   subpath <- strftime(lubridate::date(date), format="%Y/%Y%m/%Y%m%d")
#   url <- paste0("https://www.ncei.noaa.gov/data/climate-forecast-system/access/operational-analysis/6-hourly-low-resolution/", subpath)
#   cmd <- paste0("wget -nc -nH --cut-dirs=7 -np -r -l3 --accept '*sfluxgrbl*.grib2' -P ", dir_ncar, " ", url)
#   system(cmd)
# }
# 
# 
# ncar.refresh_files <- function(years){
#   
#   downloaded <- ncar.downloaded_dates()
#   downloadable <- ncar.downloadable_dates(years)
#   to_download <- downloadable[!downloadable %in% downloaded]
#   
#   lapply(to_download, ncar.download_date)
# }


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

cfs.add_pbl <- function(weather, vars=c("pbl_min","pbl_max")){
  
  dates <- weather %>%
    as.data.frame() %>%
    tidyr::unnest(weather) %>%
    distinct(date) %>%
    pull()
  
  cfs.refresh_files(dates)
  
  stations_sf <- st_as_sf(weather %>% ungroup() %>%
                            dplyr::select(station_id, geometry) %>%
                            dplyr::distinct(station_id, .keep_all=T))
  
  
  dir_cfs <- cfs.folder_cfs()
  dir_pbl <- cfs.folder_pbl()
  
  dates <- weather %>%
    as.data.frame() %>%
    rowwise() %>%
    mutate(date=list(unique((weather$date)))) %>%
    dplyr::select(station_id, timezone, date, geometry) %>%
    tidyr::unnest(date) %>%
    distinct(station_id, timezone, date, geometry) %>%
    rowwise()
  
  # Collecting all positions to be read for each file
  # And group so we can use raster::stack
  dates_files <- dates %>%
    rowwise() %>%
    mutate(fp=list(cfs.date_to_filepaths(date, dir_pbl, vars)),
           date_group=strftime(date,"%Y")) %>%
    tidyr::unnest(fp) %>%
    tidyr::nest(data=-date_group)
    
  
  process_date_group <- function(data){
   
    fps <- data %>% distinct(fp, date, variable) %>%
      filter(file.exists(fp))
    
    gs <- sf::st_as_sf(data %>% distinct(station_id,geometry))
    ts <- terra::rast(fps$fp)
    pbls <- terra::extract(ts, st_coordinates(gs))
    
    
    pbls.df <- data.frame(value=t(pbls))
    names(pbls.df) <- gs$station_id
    pbls.df$variable <- fps$variable
    pbls.df$date <- fps$date
  
    pbls.tojoin <- tibble(pbls.df) %>%
      tidyr::pivot_longer(cols=-c(date, variable), names_to="station_id", values_to="value") %>%
      tidyr::pivot_wider(names_from=variable, values_from=value)
    
    data %>%
      distinct(station_id, date) %>%
      dplyr::left_join(pbls.tojoin)
  }
  
  print(paste("Extracting pbl..."))
  pbl_values <- do.call("rbind", pbapply::pblapply(dates_files$data, process_date_group))
  print(paste("Done"))
  
  # Join to weather data
  joined <- weather %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(weather)) %>%
    dplyr::mutate(weather_station_id=station_id,
                  weather=list(weather %>% left_join(
                    pbl_values %>% dplyr::filter(station_id==weather_station_id) %>%
                      select(-c(station_id))
    ))) %>% dplyr::select(-c(weather_station_id))
  
  return(joined)
}


