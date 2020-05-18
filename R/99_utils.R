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
    dplyr::summarize(date_min=min(date), date_max = max(date)) %>%
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
  data %>% left_join(city_corr %>% select(station_id, city))
}