era5.add_weather <- function(location_dates,
                             weather_vars=c('pbl_min', 'pbl_max')){
  
  
  # Where Stefanos needs to implement code
  
  
}



era5.folder_era5 <- function(){
  dir_era5 <- Sys.getenv("DIR_ERA5")
  if(dir_era5==""){
    stop("Missing DIR_ERA5 folder")
  }
  return(dir_era5)
}


era5.processed_dates <- function(){
  dir_era5 <- era5.folder_era5()
  ds <- list.files(path = dir_era5, full.names = F, recursive = TRUE)
  dates <- unique(lubridate::ymd(ds))
  dates <- dates[!is.na(dates)]
  return(dates)
}


era5.processable_dates <- function(dates){
  # Returns list of dates (in dates arguement) that are available on CDS

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
    # Create raster file
    
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
era5.get_weather_vars <- function(dates,
                                  locations,
                                  vars=c("temp_min", ...., "pbl_min","pbl_max")){
  
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
  # 
  # 
  # dates <- weather %>%
  #   as.data.frame() %>%
  #   rowwise() %>%
  #   mutate(date=list(unique((weather$date)))) %>%
  #   dplyr::select(location_id, timezone, date, geometry) %>%
  #   tidyr::unnest(date) %>%
  #   distinct(location_id, timezone, date, geometry) %>%
  #   rowwise()
  # 
  # # Collecting all positions to be read for each file
  # # And group so we can use raster::stack
  # dates_files <- dates %>%
  #   rowwise() %>%
  #   mutate(fp=list(era5.date_to_filepaths(date, dir_pbl, vars)),
  #          date_group=strftime(date,"%Y")) %>%
  #   tidyr::unnest(fp) %>%
  #   tidyr::nest(data=-date_group)
  # 
  # 
  # process_date_group <- function(data){
  #   
  #   fps <- data %>% distinct(fp, date, variable) %>%
  #     filter(file.exists(fp))
  #   
  #   gs <- sf::st_as_sf(data %>% distinct(location_id,geometry))
  #   ts <- terra::rast(fps$fp)
  #   pbls <- terra::extract(ts, st_coordinates(gs))
  #   
  #   
  #   pbls.df <- data.frame(value=t(pbls))
  #   names(pbls.df) <- gs$location_id
  #   pbls.df$variable <- fps$variable
  #   pbls.df$date <- fps$date
  #   
  #   pbls.tojoin <- tibble(pbls.df) %>%
  #     tidyr::pivot_longer(cols=-c(date, variable), names_to="location_id", values_to="value") %>%
  #     tidyr::pivot_wider(names_from=variable, values_from=value)
  #   
  #   data %>%
  #     distinct(location_id, date) %>%
  #     dplyr::left_join(pbls.tojoin)
  # }
  # 
  # print(paste("Extracting pbl..."))
  # pbl_values <- do.call("rbind", pbapply::pblapply(dates_files$data, process_date_group))
  # print(paste("Done"))
  # 
  # # Join to weather data
  # joined <- weather %>%
  #   dplyr::rowwise() %>%
  #   dplyr::filter(!is.null(weather)) %>%
  #   dplyr::mutate(weather_location_id=location_id,
  #                 weather=list(weather %>% left_join(
  #                   pbl_values %>% dplyr::filter(location_id==weather_location_id) %>%
  #                     select(-c(location_id))
  #                 ))) %>% dplyr::select(-c(weather_location_id))
  
  return(joined)
}


