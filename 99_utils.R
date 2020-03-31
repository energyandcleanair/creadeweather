utils.add_lag <- function(meas, lag_cols, group_cols, lags, lag_unit){
  
  # First ensure it is 'hour-complete'
  print("Completing hours before 'lagging'")
  date_grid <- meas %>% dplyr::group_by_at(group_cols) %>%
    dplyr::summarize(date_min=min(date), date_max = max(date)) %>%
    dplyr::mutate(date=purrr::map2(date_min, date_max, ~seq(.x, .y, by=lag_unit))) %>%
    dplyr::select(-c(date_min, date_max)) %>%
    tidyr::unnest(cols=c(date))
  
  meas_full <- merge(meas, date_grid, by = c(group_cols,'date'), all=TRUE)
  
  result <- meas_full %>%
    group_by_at(vars(all_of(group_cols))) %>% arrange(date)
  for(lag in lags){
    print(paste("Adding", lag, lag_unit,"lag"))
    my_lag <- list(function(x) dplyr::lag(x, n=lag))
    names(my_lag) <- paste(lag) #will be appended to column name by mutate_at
    result <- result %>% mutate_at(lag_cols, my_lag)
  }
  
  
  return(result)
}
