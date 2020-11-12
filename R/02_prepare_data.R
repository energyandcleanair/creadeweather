#' Title
#'
#' @param meas_weather
#'
#' @return
#' @export
#'
#' @examples
prep_data <- function(meas_weather, filename=NULL){
  
  cache_folder <- file.path('data', '02_prep_training', 'cache')
  if(!dir.exists(cache_folder)) dir.create(cache_folder, recursive = T)
  
  output_folder <- file.path('data', '02_prep_training', 'output')
  if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
  
  input_folder <- file.path('data', '02_prep_training', 'input')
  if(!dir.exists(input_folder)) dir.create(input_folder, recursive = T)
  
  # Filter outliers etc.
  clean_meas_tbl <- function(tbl){
    
    # Remove infs in all vars
    tbl <- tbl %>% dplyr::mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x))
    
    # Apply mad filtering to pollutant peaks (very useful for e.g. sand storms in Beijing)
    n_mad <- 10
    mad <- stats::mad(tbl$value, na.rm=T)
    n_before <- nrow(tbl)
    tbl <- tbl %>% dplyr::filter(value-median(value, na.rm=T) < n_mad * mad)
    n_after <- nrow(tbl)
    if(n_before!=n_after){
      print(paste(n_before-n_after,"/", n_before,"outliers removed. Threshold:", n_mad*mad+median(tbl$value)))
    }
    return(tbl)
  }
  
  
  # Certain stations miss certain weather variables
  coalesce_weather_tbl <- function(tbl){
    
    # Different kind of treatments
    # We only coalesce weather data where other weather data exists
    weather_rows_idxs <- which(apply(tbl %>% dplyr::select(wd, precip, ceil_hgt, atmos_pres, wd, visibility, air_temp), 1, function(x) any(!is.na(x))))
  
    if(length(weather_rows_idxs)>0){
      
      # If only some of them are NAs
      max_gap = 5
      try(tbl <- tbl %>% mutate(sunshine=zoo::na.approx(sunshine, date, na.rm=FALSE, maxgap=max_gap)) %>% ungroup(), TRUE)
      try(tbl <- tbl %>% mutate(atmos_pres=zoo::na.approx(atmos_pres, date, na.rm=FALSE, maxgap=max_gap)) %>% ungroup(), TRUE)
      try(tbl <- tbl %>% mutate(air_temp_min=zoo::na.approx(air_temp_min, date, na.rm=FALSE, maxgap=max_gap)) %>% ungroup(), TRUE)
      try(tbl <- tbl %>% mutate(air_temp_max=zoo::na.approx(air_temp_max, date, na.rm=FALSE, maxgap=max_gap)) %>% ungroup(), TRUE)
      try(tbl <- tbl %>% mutate(air_temp=zoo::na.approx(air_temp, date, na.rm=FALSE, maxgap=max_gap)) %>% ungroup(), TRUE)
      
      
      # If all NAs
      fill_value <- function(v){
        # if(all(is.na(v)) | ! (0 %in% unique(v))) v[is.na(v)] <- 0
        if(all(is.na(v))) v[is.na(v)] <- 0
        v
      }

      tbl <- tbl %>% mutate_at(c("ws","precip","ceil_hgt",
                                 "atmos_pres","wd","visibility",
                                 "air_temp_min","air_temp_max","air_temp"),
                               fill_value)
      
      if('sunshine' %in% colnames(tbl)) tbl$sunshine <- fill_value(tbl$sunshine)
      
      
      
      # tbl[weather_rows_idxs,]$precip <- coalesce(tbl[weather_rows_idxs,]$precip, 0) # Precip is a bit special
    }
    
    return(tbl)
  }
  
  # Add wind direction factor
  enrich_weather_tbl <- function(tbl){
    tbl$wd_factor <- factor(tbl$wd %/% 45)
    return(tbl)
  }
  
  meas_weather <- meas_weather %>% dplyr::rowwise() %>%
    mutate(meas_weather=list(clean_meas_tbl(meas_weather))) %>%
    mutate(meas_weather=list(coalesce_weather_tbl(meas_weather))) %>%
    mutate(meas_weather=list(enrich_weather_tbl(meas_weather)))
 
  # Replace NaNs with NA (gbm doesn't like NaNs)
  meas_weather <- meas_weather %>% rowwise() %>%
    mutate(meas_weather=list(utils.replace_nan_with_na(meas_weather)))
  
  if(!is.null(filename)){
    saveRDS(meas_weather, file.path(output_folder, filename))  
  }

  return(meas_weather)
}

