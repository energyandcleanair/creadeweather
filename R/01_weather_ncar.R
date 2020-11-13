
ncar.folder_cdas1 <- function(){
  dir_ncar<- Sys.getenv("DIR_CDAS1")
  if(dir_ncar==""){
    dir_ncar = utils.get_dir_data("cdas1")
  }
  
  dir.create(dir_ncar, showWarnings = F, recursive = T)
  
  return(dir_ncar)
}

ncar.folder_pbl <- function(){
  dir_ncar<- Sys.getenv("DIR_PBL")
  if(dir_ncar==""){
    dir_ncar = utils.get_dir_data("pbl")
  }
  
  dir.create(dir_ncar, showWarnings = F, recursive = T)
  
  return(dir_ncar)
}

# 
# ncar.downloaded_dates <- function(){
#   dir_ncar <- ncar.folder()
#   ds <- list.dirs(path = dir_ncar, full.names = F, recursive = TRUE)
#   dates <- lubridate::ymd(ds)
#   dates <- dates[!is.na(dates)]
#   return(dates)
# }
# 
# ncar.downloadable_dates <- function(years){
#   l <- lapply(years,
#          function(y){seq(min(lubridate::today(),lubridate::date(paste0(y,"-01-01"))),
#                          min(lubridate::today(), lubridate::date(paste0(y,"-12-31"))),
#                          by="d")})
#   
#   tibble::tibble(l) %>% tidyr::unnest(l) %>% dplyr::pull()
# }
# 
# 
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


ncar.add_pbl <- function(weather, years){
  
  # ncar.refresh_files(years)
  
  stations_sf <- st_as_sf(weather %>% ungroup() %>%
                            dplyr::select(station_id, geometry) %>%
                            dplyr::distinct(station_id, .keep_all=T))
  
  
  folder <- ncar.folder_pbl()
  files <- list.files(folder, "*.grb2$", full.names = T)
  pat = 'cdas1\\.(\\d{8})\\.'
  dates <- lubridate::ymd(stringr::str_match(files, pat)[, 2])
  
  process_file <- function(file, stations_sf){
    r <- raster::brick(file)
    pat = 'cdas1\\.(\\d{8})\\.'
    date_file <- lubridate::ymd(stringr::str_match(file, pat)[, 2])
    # For some reason, on GCP/Ubuntu, raster is offset by 360deg
    if(raster::xmin(r)>180){
      raster::xmin(r)=raster::xmin(r)-360
      raster::xmax(r)=raster::xmax(r)-360
    }
    file_values <- as.data.frame(raster::extract(r,
                                                 stations_sf,
                                                 # buffer=500, # 500m radius
                                                 fun=mean,
                                                 na.rm=T,
                                                 sp = TRUE,
                                                 method='simple')) %>%
      dplyr::select(-c(coords.x1, coords.x2)) %>%
      tidyr::gather("key","pbl",-station_id) %>%
      group_by(station_id) %>%
      dplyr::summarize(pbl_min=min(pbl, na.rm=T),
                pbl_max=max(pbl, na.rm=T)) %>%
      dplyr::mutate(date=date_file)
    
    return(file_values)
  }
  if(is.null(years)){
    files_years <- files  
  }else{
    files_years <- files[lubridate::year(dates) %in% years]
  }
  
  pbl_values <- do.call('bind_rows', pbapply::pblapply(files_years, process_file, stations_sf=stations_sf))
  
  # Join to weather data
  joined <- weather %>% dplyr::rowwise() %>% dplyr::filter(!is.null(weather)) %>%
    dplyr::mutate(weather_station_id=station_id, weather=list(weather %>% left_join(
      pbl_values %>% dplyr::filter(station_id==weather_station_id)
    ))) %>% dplyr::select(-c(weather_station_id))
  
  return(joined)
}


