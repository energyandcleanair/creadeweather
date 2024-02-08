era5.weather_vars <- function(){
  # r <- raster::brick(era5.date_to_filepath("2020-10-03"))
  # return(names(r))
  return(c('pbl_min', 'pbl_max', 'total_precip'))
}

era5.date_to_filepath <- function(date, extension='tiff'){
  fname <- paste0('era5_', strftime(as.Date(date), "%Y-%m-%d"), '.', extension)
  file.path(era5.folder_era5(), fname)
}

era5.folder_era5 <- function(){
  dir_era5 <- utils.get_env("DIR_ERA5")
  if(dir_era5==""){
    stop("Missing DIR_ERA5 folder")
  }
  return(dir_era5)
}

extract_date <- function(fname){
  return(strsplit(strsplit(fname, split =  "[_]")[[1]][2], split = "[.]")[[1]][1])
}

era5.processed_dates <- function(){
  dir_era5 <- era5.folder_era5()
  ds <- list.files(path = dir_era5,
                   pattern = "*.tif",
                   full.names = F,
                   recursive = TRUE)
  dates <- unique(lubridate::ymd(lapply(as.list(ds), extract_date)))
  # dates <- unique(lapply(as.list(ds), extract_date))
  date <- dates[!is.na(dates)]
  return(c(date))
}

era5.processable_dates <- function(dates){
  # Returns list of dates (in dates argument) that are available on CDS
  # All dates are available on CDS (few exceptions, but no easy way to get in advance)
  return(dates[dates < lubridate::today() - 3])
}


#' Build global daily tif files with temp_min, temp_max, temp_mean, RH_min, RH_max etc.
#' And result files are saved in dir_era5
#'
#' @param date 
#' @param cache_rs 
#'
#' @return
#' @export
#'
#' @examples
era5.process_date <- function(date){
  tryCatch({
    era5_long_vars <- c('boundary_layer_height', '10m_u_component_of_wind',
                       '10m_v_component_of_wind', '2m_temperature', 
                       'maximum_2m_temperature_since_previous_post_processing',
                       'minimum_2m_temperature_since_previous_post_processing',
                       'surface_pressure', 'total_precipitation')

    fname <- paste0('era5_', date, '.nc')
    year <- strsplit(as.character(date), split =  "[-]")[[1]][1]
    month <- strsplit(as.character(date), split =  "[-]")[[1]][2]
    day <- strsplit(as.character(date), split =  "[-]")[[1]][3]
    request <- list(
      dataset_short_name = "reanalysis-era5-single-levels",
      product_type   = "reanalysis",
      format = "netcdf",
      variable = era5_long_vars,
      year = year,
      month = month,
      day = day,
      time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00",
               "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
               "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
      # area is specified as N, W, S, E
      area = c(90, -180, -90, 180),
      target = paste0(fname)
    )
    
    if(! "ecmwfr" %in% keyring::keyring_list()$keyring){
      keyring::keyring_create(keyring="ecmwfr", password = utils.get_env('KEYRING_PASSWORD'))  
    }
    
    try(keyring::keyring_unlock(keyring="ecmwfr", password = utils.get_env('KEYRING_PASSWORD')))
    
    ecmwfr::wf_set_key(user = utils.get_env('CDS_UID', error_if_not_found = T),
                       key = utils.get_env('CDS_API_KEY', error_if_not_found = T),
                       service = "cds")
    
    ecmwfr::wf_request(user = utils.get_env('CDS_UID'),
               request = request,
               transfer = TRUE,
               path = era5.folder_era5(),
               time_out=600, #shorten timeout
               verbose = TRUE) -> req
    
    layers_dict = list()
    
    calc_raster <- function(fname, var, fun){
      raster::calc(raster::brick(file.path(era5.folder_era5(), fname), var=var),
                   fun=fun,
                   na.rm=T)
    }
    
    layers_dict['pbl'] <- calc_raster(fname, 'blh', mean)
    layers_dict['pbl_min'] <- calc_raster(fname, 'blh', min)
    layers_dict['pbl_max'] <- calc_raster(fname, 'blh', max)
    layers_dict['temp'] <- calc_raster(fname, 't2m', mean)
    layers_dict['temp_max'] <- calc_raster(fname, 't2m', max)
    layers_dict['temp_min'] <- calc_raster(fname, 't2m', min)

    # Wind direction and wind speed calculation
    uv2wdws <- function(s) {
      degrees <- function(radians) 180 * radians / pi
      mathdegs <- degrees(atan2(s[2], s[1]))
      wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
      wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
      ws <- sqrt(s[1]^2 + s[2]^2)
      return(cbind(wd, ws))
    }
    u10 <- calc_raster(fname, 'u10', mean)
    v10 <- calc_raster(fname, 'v10', mean)
    wind_stack <- raster::stack(u10, v10)
    
    wdws <- raster::calc(wind_stack, uv2wdws)
    
    wd <- raster::subset(wdws, 1)
    ws <- raster::subset(wdws, 2)
      
    layers_dict['ws'] <- ws
    layers_dict['wd'] <- wd
    
    # Humidity calculation: https://confluence.ecmwf.int/pages/viewpage.action?pageId=171411214
    Rdry <- 287.0597 # Constant for dry air J kg−1 K−1
    Rvap <- 461.5250 # Constant for water vapor J kg−1 K−1
    # a1, a3, a4, and T0:
    # Parameters for Teten's formula according to Buck (1981) 
    # Buck, A. L. (1981). New equations for computing vapor pressure and
    # enhancement factor. J. Appl. Meteorol., 20, 1527–1532
    a1 <- 611.21       # units: Pa
    a3 <- 17.502       # units: K
    a4 <- 32.19        # units: K
    T0 <- 273.16       # units: K
    
    humidity_fun <- function(t2msp){
      E <- exp(17.502 *((t2msp[1]-273.16 )/(t2msp[1]-32.19)))
      
      huss = (0.6219808 * E) / (t2msp[2] - ((1-Rdry/Rvap)* E))
      # ps-((1-Rdry/Rvap)*E)
      return(huss)
    }
    
    sp <- calc_raster(fname, 'sp', mean)
    tp <- calc_raster(fname, 'tp', sum)
    t2m <- layers_dict['temp']
    t2msp <- raster::stack(c(t2m, sp))
    huss <- raster::calc(t2msp, humidity_fun)
    layers_dict['sp'] <- NULL
    layers_dict['humid'] <- huss
    layers_dict['total_precip'] <- tp
    
    output_path <- file.path(era5.folder_era5(), paste0(strsplit(fname, split =  "[.]")[[1]][1], '.tiff'))
    
    tif <- raster::stack(layers_dict)
    names(tif) <- names(layers_dict)
    
    terra_ras <- terra::rast(tif)
    t <- terra::writeRaster(terra_ras, output_path, overwrite=TRUE)
    file.remove(file.path(era5.folder_era5(), fname))
  },
  error=function(c){
    warning(paste("Failed for date", date))
    return(NULL)
  })
}



#' Generate the missing files for indicated dates
#'
#' @param dates 
#'
#' @return
#' @export
#'
#' @examples
era5.refresh_files <- function(dates){
  
  processed <- era5.processed_dates()
  processable <- era5.processable_dates(dates)
  to_process <-  processable[!processable %in% processed]
  
  cache_rs=NULL
  for(i in seq_along(to_process)){
    d <- to_process[i]
    print(paste("Processing", d))
    era5.process_date(d)
  }
}

era5.date_to_filepaths <- function(date, dir_era5){
  # returns the path to the daily file for date `date`
  return(file.path(dir_era5, paste0('era5_', date, '.tiff')))
}



#' Title
#'
#' @param dates: a list of dates for which we want weather data
#' @param locations: a dataframe with location_id and geometry columns
#' @param vars: the list of variables we are interested in
#'
#' @return a data frame with location_id, geometry, date, temp_min, temp_max ...
#' @export
#'
#' @examples
era5.add_weather <- function(weather, weather_vars){
  dates <- weather %>%
    as.data.frame() %>%
    tidyr::unnest(weather) %>%
    distinct(date) %>%
    pull()
  
  print("=== Refreshing ERA5 files ===")
  era5.refresh_files(dates)
  print("=== Done ===")
  
  stations_sf <- st_as_sf(weather %>%
                            ungroup() %>%
                            dplyr::select(location_id, geometry) %>%
                            dplyr::distinct(location_id, .keep_all=T))
  
  
  dir_era5 <- era5.folder_era5()
  
  print("=== Extracting ERA5 data at locations ===")
  
  coords <- terra::vect(stations_sf)
  era5_data <- pbmcapply::pbmclapply(dates, function(date){
    tryCatch({
      tif_file <- era5.date_to_filepaths(date, dir_era5)
      if(!file.exists(tif_file)) return(NULL)
      tif <- terra::rast(tif_file, lyrs=weather_vars)
      tb <- tibble('date'= date)
      extracted_values <- terra::extract(tif, coords)
      dplyr::bind_cols(location_id=coords$location_id, tb, as.data.frame(extracted_values) %>% dplyr::select(-c(ID)))
    }, error=function(error){return(NULL)})
  }, mc.cores = parallel::detectCores() - 1) %>%
    bind_rows
    
  
  for(location_id in coords$location_id){
    df <- merge(x=weather[weather$location_id==location_id,]$weather[[1]], 
                y=era5_data[era5_data$location_id==location_id,] %>% dplyr::select(-c(location_id)),
                by='date', all.x=TRUE)  
    weather[weather$location_id==location_id,]$weather[[1]] <- tibble(df)
  }
  
  
  print("=== Done ===")
  
  return(weather)

}