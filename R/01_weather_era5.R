era5.folder_era5 <- function(){
  dir_era5 <- Sys.getenv("DIR_ERA5")
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
  ds <- list.files(path = dir_era5, full.names = F, recursive = TRUE)
  dates <- unique(lubridate::ymd(lapply(as.list(ds), extract_date)))
  # dates <- unique(lapply(as.list(ds), extract_date))
  date <- dates[!is.na(dates)]
  return(c(date))
}

era5.processable_dates <- function(dates){
  # Returns list of dates (in dates argument) that are available on CDS
  # All dates are available on CDS (few exceptions, but no easy way to get in advance)
  return(dates)
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
    
    wf_request(user = Sys.getenv('uid'),
               request = request,
               transfer = TRUE,
               path = era5.folder_era5(),
               verbose = TRUE)
    
    layers_dict = list()
    layers_dict['pbl'] <- mean(brick(file.path(era5.folder_era5(), 
                                                    fname), var='blh'))
    layers_dict['pbl_min'] <- min(brick(file.path(era5.folder_era5(), 
                                                  fname), var='blh'))
    layers_dict['pbl_max'] <- max(brick(file.path(era5.folder_era5(), 
                                                  fname), var='blh'))
    layers_dict['mean_temp'] <- mean(brick(file.path(era5.folder_era5(), 
                                                   fname), var='t2m'))
    layers_dict['max_temp'] <- max(brick(file.path(era5.folder_era5(), 
                                                   fname), var='t2m'))
    layers_dict['min_temp'] <- min(brick(file.path(era5.folder_era5(), 
                                                   fname), var='t2m'))

    # Wind direction and wind speed calculation
    uv2wdws <- function(s) {
      degrees <- function(radians) 180 * radians / pi
      mathdegs <- degrees(atan2(s[2], s[1]))
      wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
      wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
      ws <- sqrt(s[1]^2 + s[2]^2)
      return(cbind(wd, ws))
    }
    u10 <- mean(brick(file.path(era5.folder_era5(), 
                                fname), var='u10'))
    v10 <- mean(brick(file.path(era5.folder_era5(), 
                                fname), var='v10'))
    wind_stack <- stack(u10, v10)
    
    wdws <- calc(wind_stack, uv2wdws)
    
    wd <- subset(wdws, 1)
    ws <- subset(wdws, 2)
      
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
    
    sp <- mean(brick(file.path(era5.folder_era5(), 
                               fname), var='sp'))
    t2m <- mean(brick(file.path(era5.folder_era5(), 
                                fname), var='t2m'))
    t2msp <- stack(t2m, sp)
    huss <- calc(t2msp, humidity_fun)
    layers_dict <- layers_dict[!names(layers_dict)=='sp']
    layers_dict['humid'] <- huss

    output_path <- file.path(era5.folder_era5(), paste0(strsplit(fname, split =  "[.]")[[1]][1], '.tiff'))
    
    tif <- stack(layers_dict)
    names(tif) <- names(layers_dict)
    
    terra_ras <- rast(tif)
    
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
era5.add_weather <- function(weather,
                                  weather_vars=c('pbl', 'pbl_min', 'pbl_max', 
                                                 'mean_temp', 'precip',
                                                 'max_temp', 'min_temp', 'ws',
                                                 'wd', 'humid')){
  dates <- weather %>%
    as.data.frame() %>%
    tidyr::unnest(weather) %>%
    distinct(date) %>%
    pull()
  
  era5.refresh_files(dates)
  
  stations_sf <- st_as_sf(weather %>%
                            ungroup() %>%
                            dplyr::select(location_id, geometry) %>%
                            dplyr::distinct(location_id, .keep_all=T))
  
  
  dir_era5 <- era5.folder_era5()
  
  
  for (i in seq_along(stations_sf$geometry)){
    x <- st_coordinates(stations_sf[i]$geometry)[1]
    y <- st_coordinates(stations_sf[i]$geometry)[2]
    coords <- cbind(x, y)
    
    
    
    main_tb <- NULL
    
    for (j in seq_along(dates)){
      tif <- brick(era5.date_to_filepaths(dates[j], dir_era5))
      
      tb <- tibble('date'= dates[j])
      
      for (k in weather_vars){
        band <- subset(tif, k)
        tb[k] <- extract(band, coords)
      }
      main_tb = bind_rows(main_tb,tb)
    }
    
    df <- merge(x=weather[weather$location_id==stations_sf$location_id]$weather, 
                y=main_tb, by='date', all.x=TRUE)
    weather[weather$location_id==stations_sf$location_id]$weather <- list(df)
    
  }
  return(weather)
  # # Join to weather data
  # joined <- weather %>%
  #   dplyr::rowwise() %>%
  #   dplyr::filter(!is.null(weather)) %>%
  #   dplyr::mutate(weather_location_id=location_id,
  #                 weather=list(weather %>% left_join(
  #                   pbl_values %>% dplyr::filter(location_id==weather_location_id) %>%
  #                     select(-c(location_id))
  #                 ))) %>% dplyr::select(-c(weather_location_id))
  
  # return(joined)
}