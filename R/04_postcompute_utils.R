postcompute.quality_filter <- function(result, rsq.threshold=NULL, mse.thresholds=list("NO2"=NULL, "PM10"=NULL, "CO"=NULL, "O3"=NULL,"PM2.5"=NULL,"SO2"=NULL)){
  result %>% dplyr::rowwise() %>% dplyr::filter(is.null(rsq.threshold) || (model$r.squared>=rsq.threshold)) %>%
    dplyr::filter(is.null(mse.thresholds[[pollutant]]) || (model$prediction.error<=mse.thresholds[[pollutant]]))
}

postcompute.before_after <- function(result_, years_before){
  average_after <- function(observations, lockdown_date, value_col='value'){
    as.double(observations %>% dplyr::filter(date >= lockdown_date) %>%
                summarise_at(value_col, mean, na.rm=T))
  }

  average_before <- function(observations, lockdown_date, years, value_col='value'){
    max_date <- max(observations$date)
    as.double(observations %>% dplyr::filter(lubridate::year(date) %in% years,
                                      lubridate::yday(date) >= lubridate::yday(lockdown_date),
                                      lubridate::yday(date) <= lubridate::yday(max_date)
    ) %>% summarise_at(value_col, mean, na.rm=T))
  }

  result_ <- result_ %>% rowwise() %>%
    dplyr::mutate(after_observed=average_after(model_fitted$observations, movement),
           before_observed=average_before(model_fitted$observations, movement, years_before),
           after_normalised=average_after(model_fitted$normalised, movement, value_col='value_predict'),
           before_normalised=average_before(model_fitted$normalised, movement, years_before, value_col='value_predict'))

  result_ <- result_ %>% dplyr::mutate(ratio_observed=(after_observed-before_observed)/before_observed,
                              ratio_normalised=(after_normalised-before_normalised)/before_normalised,
                              diff_observed=after_observed-before_observed,
                              diff_normalised=after_normalised-before_normalised
  )

  result_ <- result_ %>% dplyr::select(station_id, country, pollutant, unit, diff_observed, diff_normalised, ratio_observed, ratio_normalised) %>%
    tidyr::gather("type","value", c(diff_observed, diff_normalised, ratio_observed, ratio_normalised)) %>%
    tidyr::separate(type, c("var", "normalised"))

  result_$years_before <- paste(years_before, collapse='_')
  result_
}

postcompute.add_metadata <- function(result_){
  
  # Add lockdown info
  lockdown <- read.csv(url('https://docs.google.com/spreadsheets/d/e/2PACX-1vTKMedY9Mzy7e81wWU95Ent79Liq7UwbUz0qTQbkSeAmFCPfqIVNbl1zs99bUOgsJUJbz53GxvBfeiP/pub?gid=0&single=true&output=csv'))
  lockdown$movement <- as.POSIXct(strptime(lockdown$movement_national,"%Y%m%d"))
  lockdown$school <- as.POSIXct(strptime(lockdown$school,"%Y%m%d"))
  lockdown$workplace <- as.POSIXct(strptime(lockdown$workplace,"%Y%m%d"))
  lockdown$school_workplace <- pmin(lockdown$school, lockdown$workplace, na.rm=T)
  lockdown$iso2 <- countrycode::countrycode(lockdown$iso3, origin='iso3c', destination='iso2c')
  lockdown$country <- as.character(lockdown$country)
  
  join_dblocs <- ! all(c("country","geometry") %in% colnames(result_))
  if(join_dblocs){
    locs <- creadb::locations(id=unique(result_$station_id))
    result_ <- result_ %>% dplyr::ungroup() %>% dplyr::left_join(locs, by=c('station_id'='id'), all.x=T)
  }
  
  result_ <- result_ %>% dplyr::ungroup() %>%
    dplyr::left_join(lockdown, by=c('country'='iso2')) %>%
    tidyr::replace_na(list('movement'='2020-03-16')) %>%
    dplyr::rename(iso2=country)
  
  if(join_dblocs){
    result_ <- result_ %>% dplyr::rename(country=country.y)
  }else{
    result_ <- result_ %>%
      dplyr::mutate(country=countrycode::countrycode(iso2, origin='iso2c', destination='country.name'),
                    country.y=NULL)
  }
  
  result_ <- result_ %>% dplyr::left_join(result_ %>% dplyr::filter(!is.na(country)) %>%
                                            dplyr::group_by(country, pollutant) %>%
                                            dplyr::summarise(country_count=n()))
  return(result_)
}

postcompute.add_gpw <- function(result_){
  gpw_ <- raster::raster(x=file.path('data','00_init','input','gpw_v4_population_density_rev11_2020_15_min.tif'))
  result_$gpw <- raster::extract(gpw_, sf::st_as_sf(result_))
  return(result_)
}


postcompute.lockdown_impact <- function(result_){
  # Calculate lockdown impact: observed vs weather predicted after lockdown
  
  average_after <- function(obs, lockdown_date, value_col='value'){
    as.double(obs %>% dplyr::filter(date >= lockdown_date) %>%
                dplyr::summarise_at(value_col, mean, na.rm=T))
  }
  
  result_ <- result_ %>% dplyr::rowwise() %>% dplyr::filter(!is.null(movement)) %>%
    dplyr::mutate(avg_observed=average_after(predicted, movement, value_col='value'),
           avg_predicted=average_after(predicted, movement, value_col='predicted'))
  
  result_ <- result_ %>% dplyr::mutate(diff_ratio=(avg_observed-avg_predicted)/avg_predicted,
                                diff=avg_observed-avg_predicted)
  
  result_
}

postcompute.population_weighted <- function(result_, group_by_cols, value_col){
  if(!'gpw' %in% colnames(result_)){
    result_ <- postcompute.add_gpw(result_)
  }
  result_ %>% dplyr::ungroup() %>% dplyr::filter(!is.na(gpw)) %>% dplyr::group_by_at(group_by_cols) %>%
    dplyr::summarise_at(value_col, ~ weighted.mean(., w=gpw, na.rm=T))
}

