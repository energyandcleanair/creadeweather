#' Function to get weekday number from a date where \code{1} is Monday and 
#' \code{7} is Sunday. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Date vector.
#' 
#' @param as.factor Should the return be a factor? 
#' 
#' @return Numeric vector.
#' 
utils.wday_monday <- function(x, as.factor = FALSE) {
  
  x <- lubridate::wday(x)
  x <- x - 1
  x <- ifelse(x == 0, 7, x)
  if (as.factor) x <- factor(x, levels = 1:7, ordered = TRUE)
  return(x)
  
}

utils.iferr <- function(code, value_if_err, silent=T){
    tryCatch(code, error = function(c) {
      if(!silent) print(c)
      value_if_err})
}

utils.add_lag <- function(meas, lag_cols, group_cols, lags, lag_unit){
  
  # First ensure it is 'hour-complete'
  date_grid <- meas %>% dplyr::group_by_at(group_cols) %>%
    dplyr::summarize(date_min=min(date, na.rm=T), date_max = max(date, na.rm=T)) %>%
    dplyr::mutate(date=purrr::map2(date_min, date_max, ~seq(.x, .y, by=lag_unit))) %>%
    dplyr::select(-c(date_min, date_max)) %>%
    tidyr::unnest(cols=c(date))
  
  meas_full <- merge(meas, date_grid, by = c(group_cols,'date'), all=TRUE)
  
  result <- meas_full %>%
    group_by_at(vars(all_of(group_cols))) %>% arrange(date)
  for(lag in lags){
    my_lag <- list(function(x) dplyr::lag(x, n=lag))
    names(my_lag) <- paste(lag) #will be appended to column name by mutate_at
    result <- result %>% dplyr::mutate_at(lag_cols, my_lag)
  }
  
  
  return(result)
}

utils.most_frequent_value <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

utils.rolling_average <- function(data, average_by, average_width, group_cols, avg_cols, max_nas=NULL){
  
  data <- data %>% dplyr::mutate(date=lubridate::floor_date(date, average_by))
  date_grid <- data %>% dplyr::group_by_at(group_cols) %>%
    dplyr::summarize(date_min=min(date), date_max = max(date)) %>%
    dplyr::filter(!is.na(date_min)) %>%
    dplyr::mutate(date=purrr::map2(date_min, date_max, ~seq(.x, .y, by=average_by))) %>%
    dplyr::select(-c(date_min, date_max)) %>%
    tidyr::unnest(cols=c(date))
  
  data <- merge(data, date_grid, by = c(group_cols, 'date'), all=TRUE)
  
  # Rolling mean for training
  mean_fn <- function(x){
    if(!is.null(max_nas) && sum(is.na(x))>max_nas){
      return(NA)
    }
      
    if(is.numeric(x)){
      res <- mean(x, na.rm = T) # it sometimes returns NaN but models expect only NA
      return(if(is.na(res)) NA else res)
    }else{
      return(utils.most_frequent_value(x))
    }
  }
  train_roll_fn <- function(var) zoo::rollapply(var, width=average_width, FUN=mean_fn, align='right', fill=NA)
  # first average per date
  data <- data %>% dplyr::group_by_at(c(group_cols, 'date')) %>%
    dplyr::summarise_at(avg_cols, mean_fn)
  
  # then rolling average
  data <- data %>% dplyr::group_by_at(group_cols) %>% dplyr::arrange(date) %>%
    dplyr::mutate_at(avg_cols, train_roll_fn)
  return(data)
}


utils.replace_nan_with_na <- function(tbl){
  list_names <- colnames(tbl)
  list_values <- rep(NA, length(list_names))
  replace_list <- as.list(list_values)
  names(replace_list) <- list_names
  tbl %>% tidyr::replace_na(replace_list)
}

utils.average_over_yearly_periods <- function(tbl, meas_col, years, doys){
  if(is.na(tbl)) return(NA)
  # doys: day of years
  (tbl %>% filter(lubridate::year(date) %in% years,
                  lubridate::yday(date) %in% doys) %>%
      group_by() %>%
      summarise_at(c(meas_col), mean, na.rm = TRUE))[[meas_col]][[1]]
}

utils.add_city <- function(data){
  city_corr <- read.csv(file.path('data','00_init','input','eea_station_city.csv'))
  data %>% left_join(city_corr %>% select(location_id, city))
}



#' Find stations in selected cities
#' Finding stations in selected cities using name fuzzy matching.
#' Also looking at GADM2 names (some large cities make a whole GADM2)
#'
#' @param locs 
#' @param cities 
#' @param manual 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
utils.attach_city_by_name <- function(locs, cities, manual=NULL, ...){
  
  if(!"iso2" %in% names(cities)){
    cities <- cities %>%
      mutate(iso2=countrycode::countrycode(country.etc, origin = 'country.name', destination = 'iso2c'))
  }
  
  c <- cities %>%
    filter(iso2 %in% unique(locs$country)) %>%
    mutate(city_clean=tolower(stringi::stri_trans_general(str=name, id = "Latin-ASCII")))
  
  locs <- locs %>%
    mutate(city_clean=tolower(stringi::stri_trans_general(str=city, id = "Latin-ASCII")),
           gadm2_clean=tolower(stringi::stri_trans_general(str=name_2, id = "Latin-ASCII")))
  
  cl <- c %>%
    fuzzyjoin::stringdist_left_join(locs, by=c("city_clean"="city_clean",
                                               "iso2"="country"), ignore_case=T, max_dist = 1)
  
  # We also try with GADM2 in case GADM2 is the city level (name_2==city)
  cl_gadm2 <- cl %>% 
    # filter(is.na(id) | name.x %in% c("Amsterdam","Stockholm","Dublin")) %>%
    fuzzyjoin::stringdist_left_join(locs %>% filter(!is.na(gadm2_clean)),
                                    by=c("city_clean.x"="gadm2_clean",
                                         "iso2"="country"), ignore_case=T, max_dist = 1)
  
  cl <- rbind(
    cl %>% dplyr::select(id, name=name.x, city, country, geometry, pop, capital),
    cl_gadm2 %>% dplyr::select(id=id.y, name=name_2.y, city=city.y, country=country.y, geometry=geometry.y, pop, capital)
  ) %>%
    distinct(id, .keep_all = T)
  
  return(cl)
}


utils.attach_city_by_location <- function(locs, cities, radius_km=20){
  
  if(!'geometry' %in% colnames(locs)){
    stop("Need geometry info")
  }
  
  cities <- cities %>%
    mutate(iso2=countrycode::countrycode(country.etc, origin = 'country.name', destination = 'iso2c')) %>%
    filter(iso2 %in% unique(locs$country))
  
  cities_sf <-
    st_transform(
      st_buffer(
        st_transform(
          st_as_sf(cities, coords = c("long", "lat"), crs = 4326) %>% mutate(center=geometry),
          3857),
        radius_km*1000),
      4326)
  
  # Take closest city
  locs_cities <- st_as_sf(locs, crs=4326) %>%
    sf::st_join(cities_sf, join=st_intersects) %>%
    mutate(distance=st_distance(geometry, center, by_element = T)) %>%
    group_by(id)  %>%
    arrange(distance) %>% 
    slice(1) %>%
    filter(!is.na(pop))
  
  return(locs_cities)
}


utils.attach_city <- function(locs, cities, method="location", ...){
  if(method=="location") return(utils.attach_city_by_location(locs=locs, cities=cities, ...))
  if(method=="name") return(utils.attach_city_by_name(locs=locs, cities=cities, ...))
}


# Paths -------------------------------------------------------------------


#' Mounted folder of CREA bucket
#'
#' @return Local path to mounted bucket
#'
#' @examples
utils.get_dir_data <- function(){
  suppressWarnings(try(dotenv::load_dot_env(), silent = T))
  
  dir_data <- Sys.getenv("DIR_DATA")
  if(dir_data==""){
    warning("DIR_DATA environment variable undefined. Using working directory.")
    dir_data = getwd()
  }
  return(dir_data)
}

#' Cache folder in mounted CREA bucket
#'
#' @param subfolder Subfolder (to create if not existing)
#' @return Local path to cache folder
#'
#' @export
utils.get_cache_folder <- function(subfolder=NULL){
  folder <- file.path(utils.get_dir_data(), "cache")
  
  if(!is.null(subfolder)){
    folder <- file.path(folder, subfolder)
  }
  
  if(!dir.exists(folder)) dir.create(folder, recursive = T)
  return(folder)
}

#' Output folder in mounted CREA bucket
#'
#' @param subfolder Subfolder (to create if not existing)
#' @return Local path to output folder
#'
#' @export
utils.get_output_folder <- function(subfolder=NULL){
  folder <- file.path(utils.get_dir_data(), "output")
  
  if(!is.null(subfolder)){
    folder <- file.path(folder, subfolder)
  }
  
  if(!dir.exists(folder)) dir.create(folder, recursive = T)
  return(folder)
}

#' Download a file only if it hasn't changed since \code{last_modified}
#' 
#' @param URL url of file
#' @param fil path to write file
#' @param last_modified \code{POSIXct}. Ideally, the output from the first 
#'        successful run of \code{get_file()}
#' @param overwrite overwrite the file if it exists?
#' @param .verbose output a message if the file was unchanged?
utils.download_file <- function(URL, fil, last_modified=NULL, overwrite=TRUE, .verbose=TRUE) {
  
  if ((!file.exists(fil)) || is.null(last_modified)) {
    res <- GET(URL, write_disk(fil, overwrite))
    return(httr::parse_http_date(res$headers$`last-modified`))
  } else if (inherits(last_modified, "POSIXct")) {
    res <- HEAD(URL)
    cur_last_mod <- httr::parse_http_date(res$headers$`last-modified`)
    if (cur_last_mod != last_modified) {
      res <- GET(URL, write_disk(fil, overwrite))
      return(httr::parse_http_date(res$headers$`last-modified`))
    }
    if (.verbose) message(sprintf("'%s' unchanged since %s", URL, last_modified))
    return(last_modified)
  } 
  
}

