#' Post compute results
#'
#' @param engine 
#' @param result_folder 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
post_compute <- function(results_nested, output, ...){
  
  results_anomaly_abs <- NULL
  results_anomaly_rel <- NULL
  results_anomaly_rel_cf <- NULL
  results_counterfactual <- NULL
  results_anomaly_yday_abs <- NULL
  results_anomaly_yday_rel <- NULL
  results_anomaly_yday_rel_cf <- NULL
  results_anomaly_offsetted <- NULL
  results_anomaly_yday_offsetted <- NULL
  results_trend <- NULL
  
  if("anomaly" %in% output){
    
    # Anomaly in absolute terms
    results_anomaly_abs <- results_nested %>% dplyr::filter(output=='anomaly') %>% tidyr::unnest(cols=c(result))  
    results_anomaly_abs <- results_anomaly_abs  %>% rowwise()  %>%
      dplyr::mutate(normalised=list(predicted %>%
                                      filter(set=='testing') %>%
                                      mutate(value=value-predicted)), # Not residuals but ANOMALY (i.e. -1 * residuals)
                    unit=paste('Δ', unit) # To force ploting on different charts on Dashboard
      ) %>%
      dplyr::rename(location_id=station_id) %>%
      dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)  
    
    # Anomaly in relative terms (vs average)
    results_anomaly_rel <- results_nested %>% dplyr::filter(output=='anomaly') %>% tidyr::unnest(cols=c(result))
    results_anomaly_rel <- results_anomaly_rel  %>% rowwise()  %>%
      dplyr::mutate(
        #  we use average during training period (i.e. 2017-2019) as reference
        average=predicted %>%
          filter(set=='training') %>%
          pull(value) %>%
          mean(na.rm=T),
        normalised=list(predicted %>%
                          filter(set=='testing') %>%
                          mutate(value=(value-predicted) / average) %>%
                          select(date, value)),
        process_deweather=stringr::str_replace(process_deweather,
                                               "\"output\":\"anomaly\"",
                                               "\"output\":\"anomaly_vs_average\""),
        output="anomaly_vs_average",
        unit="-"
      ) %>%
      dplyr::rename(location_id=station_id) %>%
      dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)
    
    # Anomaly in relative terms (vs counterfactual)
    results_anomaly_rel_cf <- results_nested %>% dplyr::filter(output=='anomaly') %>% tidyr::unnest(cols=c(result))
    results_anomaly_rel_cf <- results_anomaly_rel_cf  %>% rowwise()  %>%
      dplyr::mutate(
        #  we use average during training period (i.e. 2017-2019) as reference
        average=predicted %>%
          filter(set=='training') %>%
          pull(value) %>%
          mean(na.rm=T),
        normalised=list(predicted %>%
                          filter(set=='testing') %>%
                          mutate(value=(value-predicted) / predicted) %>%
                          select(date, value)),
        process_deweather=stringr::str_replace(process_deweather,
                                               "\"output\":\"anomaly\"",
                                               "\"output\":\"anomaly_vs_counterfactual\""),
        output="anomaly_vs_counterfactual",
        unit="-"
      ) %>%
      dplyr::rename(location_id=station_id) %>%
      dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)
    
    # Anomaly offsetted
    results_anomaly_offsetted <- results_nested %>% dplyr::filter(output=='anomaly') %>% tidyr::unnest(cols=c(result))
    results_anomaly_offsetted <- results_anomaly_offsetted  %>% rowwise()  %>%
      dplyr::mutate(
        # offset is basically the mean of values during training period (i.e. 2017-2019)
        offset=predicted %>%
          filter(set=='training') %>%
          pull(value) %>%
          mean(na.rm=T),
        normalised=list(predicted %>%
                          filter(set=='testing') %>%
                          mutate(value=value-predicted+offset) %>%
                          select(date, value)),
        process_deweather=stringr::str_replace(process_deweather,
                                               "\"output\":\"anomaly\"",
                                               "\"output\":\"anomaly_offsetted\""),
        output="anomaly_offsetted"
      ) %>%
      dplyr::rename(location_id=station_id) %>%
      dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)
    
    # Counterfactual
    results_counterfactual <- results_nested %>% dplyr::filter(output=='anomaly') %>% tidyr::unnest(cols=c(result))
    results_counterfactual <- results_counterfactual  %>% rowwise()  %>%
      dplyr::mutate(normalised=list(predicted %>%
                                      mutate(value=predicted)),
                    process_deweather=stringr::str_replace(process_deweather,
                                                           "\"output\":\"anomaly\"",
                                                           "\"output\":\"counterfactual\""),
                    output="counterfactual"
      ) %>%
      dplyr::rename(location_id=station_id) %>%
      dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)  
    
  }
  
  if("anomaly_yday" %in% output){
    
    # Anomaly in absolute terms
    results_anomaly_yday_abs <- results_nested %>% dplyr::filter(output=='anomaly_yday') %>% tidyr::unnest(cols=c(result))  
    results_anomaly_yday_abs <- results_anomaly_yday_abs  %>% rowwise()  %>%
      dplyr::mutate(normalised=list(predicted %>%
                                      filter(set=='testing') %>%
                                      mutate(value=value-predicted)), # Not residuals but ANOMALY (i.e. -1 * residuals)
                    unit=paste('Δ', unit) # To force ploting on different charts on Dashboard
      ) %>%
      dplyr::rename(location_id=station_id) %>%
      dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)  
    
    # Anomaly in relative terms
    results_anomaly_yday_rel <- results_nested %>% dplyr::filter(output=='anomaly_yday') %>% tidyr::unnest(cols=c(result))
    results_anomaly_yday_rel <- results_anomaly_yday_rel  %>% rowwise()  %>%
      dplyr::mutate(
        #  we use average during training period (i.e. 2017-2019) as reference
        average=predicted %>%
          filter(set=='training') %>%
          pull(value) %>%
          mean(na.rm=T),
        normalised=list(predicted %>%
                          filter(set=='testing') %>%
                          mutate(value=(value-predicted) / average) %>%
                          select(date, value)),
        process_deweather=stringr::str_replace(process_deweather,
                                               "\"output\":\"anomaly_yday\"",
                                               "\"output\":\"anomaly_yday_vs_average\""),
        output="anomaly_yday_vs_average",
        unit="-"
      ) %>%
      dplyr::rename(location_id=station_id) %>%
      dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)
    
    # Anomaly in relative terms (vs counterfactual)
    results_anomaly_yday_rel_cf <- results_nested %>% dplyr::filter(output=='anomaly_yday') %>% tidyr::unnest(cols=c(result))
    results_anomaly_yday_rel_cf <- results_anomaly_yday_rel_cf  %>% rowwise()  %>%
      dplyr::mutate(
        #  we use average during training period (i.e. 2017-2019) as reference
        average=predicted %>%
          filter(set=='training') %>%
          pull(value) %>%
          mean(na.rm=T),
        normalised=list(predicted %>%
                          filter(set=='testing') %>%
                          mutate(value=(value-predicted) / predicted) %>%
                          select(date, value)),
        process_deweather=stringr::str_replace(process_deweather,
                                               "\"output\":\"anomaly_yday\"",
                                               "\"output\":\"anomaly_yday_vs_counterfactual\""),
        output="anomaly_yday_vs_counterfactual",
        unit="-"
      ) %>%
      dplyr::rename(location_id=station_id) %>%
      dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)
    
    # Anomaly offsetted
    results_anomaly_yday_offsetted <- results_nested %>% dplyr::filter(output=='anomaly_yday') %>% tidyr::unnest(cols=c(result))
    results_anomaly_yday_offsetted <- results_anomaly_yday_offsetted  %>% rowwise()  %>%
      dplyr::mutate(
        # offset is basically the mean of values during training period (i.e. 2017-2019)
        offset=list(predicted %>%
                      filter(set=='training') %>%
                      mutate(yday=lubridate::yday(date)) %>%
                      group_by(yday) %>%
                      summarise(offset=mean(value, na.rm=T))),
        normalised=list(predicted %>% 
                          filter(set=='testing') %>%
                          mutate(yday=lubridate::yday(date)) %>%
                          merge(offset) %>%
                          mutate(value=value-predicted+offset) %>%
                          select(date,value)),
        process_deweather=stringr::str_replace(process_deweather,
                                               "\"output\":\"anomaly_yday\"",
                                               "\"output\":\"anomaly_yday_offsetted\""),
        output="anomaly_yday_offsetted"
      ) %>%
      dplyr::rename(location_id=station_id) %>%
      dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)  
  }
  
  if("trend" %in% output){
    results_trend <- results_nested %>% dplyr::filter(output=='trend') %>% tidyr::unnest(cols=c(result))
    results_trend <- results_trend  %>% rowwise()  %>%
      dplyr::mutate(normalised=list(trend)) %>%
      dplyr::rename(location_id=station_id) %>%
      dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)
  }
  
  results <- dplyr::bind_rows(
    results_trend,
    results_anomaly_abs,
    results_anomaly_rel,
    results_anomaly_rel_cf,
    results_anomaly_offsetted,
    results_counterfactual,
    results_anomaly_yday_abs,
    results_anomaly_yday_rel,
    results_anomaly_yday_rel_cf,
    results_anomaly_yday_offsetted
  )  
  
  return(results)
}


# Group by GADM2 and GADM1
# results_anomaly_gadm1
# process_id replace city->gadm
post_agg_gadm1 <- function(results, locs){
  results %>%
    dplyr::left_join(locs %>% dplyr::select(city, gid_1), by=c("location_id"="city")) %>%
    dplyr::mutate(process_id=gsub("city","gadm1",process_id)) %>%
    tidyr::unnest(cols=normalised) %>%
    dplyr::group_by(process_id, process_deweather, poll, unit, source, gid_1, date, output) %>%
    dplyr::summarise(value=mean(value, na.rm=T)) %>%
    tidyr::nest() %>%
    rename(location_id=gid_1, normalised=data)
}

post_agg_gadm2 <- function(results, locs){
  results %>%
    left_join(locs %>% dplyr::select(city, gid_1, gid_2), by=c("location_id"="city")) %>%
    mutate(process_id=gsub("city","gadm2",process_id)) %>%
    tidyr::unnest(cols=normalised) %>%
    group_by(process_id, process_deweather, poll, unit, source, gid_2, date, output) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    tidyr::nest() %>%
    rename(location_id=gid_2, normalised=data)
}

