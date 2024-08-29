
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

postcompute <- function(results){
  mapply(postcompute_one,
         models=results$models,
         performances=results$performances,
         data=results$data,
         config=results$config,
         location_id=results$location_id,
         poll=results$poll,
         unit=results$unit,
         source=results$source,
         process_id=results$process_id,
         USE.NAMES=F,
         SIMPLIFY=FALSE
         ) %>%
    do.call(bind_rows, .)
}



postcompute_one <- function(models, data, config, location_id, poll, unit, source, process_id, performances, ...){
  
  engine <- config$engine
  keep_models <- config$keep_models
  
  if(engine=='gbm'){
    result <- postcompute_gbm(models=models, data=data, config=config, performances=performances, ...) %>%
      mutate(location_id=location_id,
             poll=poll,
             unit=unit,
             source=source,
             process_id=process_id)
  }
  
  # results_anomaly_abs <- NULL
  # results_anomaly_rel <- NULL
  # results_anomaly_rel_cf <- NULL
  # results_counterfactual <- NULL
  # results_anomaly_offsetted <- NULL
  # results_counterfactual_nofire <- NULL
  # 
  # results_anomaly_yday_abs <- NULL
  # results_anomaly_yday_rel <- NULL
  # results_anomaly_yday_rel_cf <- NULL
  # results_anomaly_yday_offsetted <- NULL
  # results_counterfactual_nofire_yday <- NULL
  # 
  # results_trend <- NULL
  # 
  # if("anomaly" %in% output){
  #   
  #   results_anomaly_raw <- results_nested %>%
  #     dplyr::filter(output=='anomaly') %>%
  #     tidyr::unnest(cols=c(result)) %>%
  #     rowwise()
  #   
  #   # Anomaly in absolute terms
  #   results_anomaly_abs <- results_anomaly_raw %>%
  #     dplyr::mutate(normalised=list(predicted %>%
  #                                     filter(set=='prediction') %>%
  #                                     mutate(observed=value) %>%
  #                                     mutate(value=value-predicted)), # Not residuals but ANOMALY (i.e. -1 * residuals)
  #                   unit=paste('Δ', unit) # To force ploting on different charts on Dashboard
  #     ) %>%
  #     dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output, model)  
  #   
  #   # Anomaly in relative terms (vs average)
  #   results_anomaly_rel <- results_anomaly_raw %>%
  #     dplyr::mutate(
  #       #  we use average during training period (i.e. 2017-2019) as reference
  #       average=predicted %>%
  #         filter(set=='training') %>%
  #         pull(value) %>%
  #         mean(na.rm=T),
  #       normalised=list(predicted %>%
  #                         filter(set=='prediction') %>%
  #                         mutate(observed=value) %>%
  #                         mutate(value=(value-predicted) / average) %>%
  #                         select(date, value, observed)),
  #       process_deweather=stringr::str_replace(process_deweather,
  #                                              "\"output\":\"anomaly\"",
  #                                              "\"output\":\"anomaly_vs_average\""),
  #       output="anomaly_vs_average",
  #       unit="-"
  #     ) %>%
  #     dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output, model)
  #   
  #   # Anomaly in relative terms (vs counterfactual)
  #   results_anomaly_rel_cf <- results_anomaly_raw %>%
  #     dplyr::mutate(
  #       #  we use average during training period (i.e. 2017-2019) as reference
  #       average=predicted %>%
  #         filter(set=='training') %>%
  #         pull(value) %>%
  #         mean(na.rm=T),
  #       normalised=list(predicted %>%
  #                         filter(set=='prediction') %>%
  #                         mutate(observed=value) %>%
  #                         mutate(value=(value-predicted) / predicted) %>%
  #                         select(date, value, observed)),
  #       process_deweather=stringr::str_replace(process_deweather,
  #                                              "\"output\":\"anomaly\"",
  #                                              "\"output\":\"anomaly_vs_counterfactual\""),
  #       output="anomaly_vs_counterfactual",
  #       unit="-"
  #     ) %>%
  #     dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output, model)
  #   
  #   # Anomaly offsetted
  #   results_anomaly_offsetted <- results_anomaly_raw %>%
  #     dplyr::mutate(
  #       # offset is basically the mean of values during training period (i.e. 2017-2019)
  #       offset=predicted %>%
  #         filter(set=='training') %>%
  #         pull(value) %>%
  #         mean(na.rm=T),
  #       normalised=list(predicted %>%
  #                         filter(set=='prediction') %>%
  #                         mutate(observed=value) %>%
  #                         mutate(value=value-predicted+offset) %>%
  #                         select(date, value, observed)),
  #       process_deweather=stringr::str_replace(process_deweather,
  #                                              "\"output\":\"anomaly\"",
  #                                              "\"output\":\"anomaly_offsetted\""),
  #       output="anomaly_offsetted"
  #     ) %>%
  #     dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output, model)
  #   
  #   # Counterfactual
  #   results_counterfactual <- results_anomaly_raw  %>%
  #     dplyr::mutate(normalised=list(predicted %>%
  #                                     mutate(observed=value) %>%
  #                                     mutate(value=predicted)),
  #                   process_deweather=stringr::str_replace(process_deweather,
  #                                                          "\"output\":\"anomaly\"",
  #                                                          "\"output\":\"counterfactual\""),
  #                   output="counterfactual"
  #     ) %>%
  #     dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output, model)  
  #   
  #   # Counterfactual no fire
  #   if(add_fire){
  #     results_counterfactual_nofire <- results_anomaly_raw %>%
  #       dplyr::mutate(normalised=list(predicted %>%
  #                                       mutate(observed=value) %>%
  #                                       mutate(value=predicted_nofire)),
  #                     process_deweather=stringr::str_replace(process_deweather,
  #                                                            "\"output\":\"anomaly\"",
  #                                                            "\"output\":\"counterfactual_nofire\""),
  #                     output="counterfactual_nofire"
  #       ) %>%
  #       dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output, model)
  #   }
  # }
  # 
  # if("anomaly_yday" %in% output){
  #   
  #   results_anomaly_yday_raw <- results_nested %>%
  #     dplyr::filter(output=='anomaly_yday') %>%
  #     tidyr::unnest(cols=c(result)) %>%
  #     rowwise()
  #   
  #   # Anomaly in absolute terms
  #   results_anomaly_yday_abs <- results_anomaly_yday_raw %>%
  #     dplyr::mutate(normalised=list(predicted %>%
  #                                     filter(set=='prediction') %>%
  #                                     mutate(observed=value) %>%
  #                                     mutate(value=value-predicted)), # Not residuals but ANOMALY (i.e. -1 * residuals)
  #                   unit=paste('Δ', unit) # To force ploting on different charts on Dashboard
  #     ) %>%
  #     dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)  
  #   
  #   # Anomaly in relative terms
  #   results_anomaly_yday_rel <- results_anomaly_yday_raw %>%
  #     dplyr::mutate(
  #       #  we use average during training period (i.e. 2017-2019) as reference
  #       average=predicted %>%
  #         filter(set=='training') %>%
  #         pull(value) %>%
  #         mean(na.rm=T),
  #       normalised=list(predicted %>%
  #                         filter(set=='prediction') %>%
  #                         mutate(observed=value) %>%
  #                         mutate(value=(value-predicted) / average) %>%
  #                         select(date, value, observed)),
  #       process_deweather=stringr::str_replace(process_deweather,
  #                                              "\"output\":\"anomaly_yday\"",
  #                                              "\"output\":\"anomaly_yday_vs_average\""),
  #       output="anomaly_yday_vs_average",
  #       unit="-"
  #     ) %>%
  #     dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)
  #   
  #   # Anomaly in relative terms (vs counterfactual)
  #   results_anomaly_yday_rel_cf <- results_anomaly_yday_raw  %>%
  #     dplyr::mutate(
  #       #  we use average during training period (i.e. 2017-2019) as reference
  #       average=predicted %>%
  #         filter(set=='training') %>%
  #         pull(value) %>%
  #         mean(na.rm=T),
  #       normalised=list(predicted %>%
  #                         filter(set=='prediction') %>%
  #                         mutate(observed=value) %>%
  #                         mutate(value=(value-predicted) / predicted) %>%
  #                         select(date, value, observed)),
  #       process_deweather=stringr::str_replace(process_deweather,
  #                                              "\"output\":\"anomaly_yday\"",
  #                                              "\"output\":\"anomaly_yday_vs_counterfactual\""),
  #       output="anomaly_yday_vs_counterfactual",
  #       unit="-"
  #     ) %>%
  #     dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)
  #   
  #   # Anomaly offsetted
  #   results_anomaly_yday_offsetted <- results_anomaly_yday_raw %>%
  #     dplyr::mutate(
  #       # offset is basically the mean of values during training period (i.e. 2017-2019)
  #       offset=list(predicted %>%
  #                     filter(set=='training') %>%
  #                     mutate(yday=lubridate::yday(date)) %>%
  #                     group_by(yday) %>%
  #                     summarise(offset=mean(value, na.rm=T))),
  #       normalised=list(predicted %>% 
  #                         mutate(yday=lubridate::yday(date)) %>%
  #                         merge(offset) %>%
  #                         mutate(observed=value) %>%
  #                         mutate(value=value-predicted+offset) %>%
  #                         select(date,value, observed)),
  #       process_deweather=stringr::str_replace(process_deweather,
  #                                              "\"output\":\"anomaly_yday\"",
  #                                              "\"output\":\"anomaly_yday_offsetted\""),
  #       output="anomaly_yday_offsetted"
  #     ) %>%
  #     dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output)  
  #   
  #   # Counterfactual no fire
  #   if(add_fire){
  #     results_counterfactual_nofire_yday <- results_anomaly_yday_raw %>%
  #       dplyr::mutate(normalised=list(predicted %>%
  #                                       mutate(observed=value) %>%
  #                                       mutate(value=predicted_nofire)),
  #                     process_deweather=stringr::str_replace(process_deweather,
  #                                                            "\"output\":\"anomaly_yday\"",
  #                                                            "\"output\":\"counterfactual_nofire_yday\""),
  #                     output="counterfactual_nofire_yday"
  #       ) %>%
  #       dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output, model)
  #   }
  # }
  # 
  # if("trend" %in% output){
  #   results_trend <- results_nested %>% dplyr::filter(output=='trend') %>% tidyr::unnest(cols=c(result))
  #   results_trend <- results_trend  %>% rowwise()  %>%
  #     dplyr::mutate(normalised=list(trend)) %>%
  #     dplyr::select(process_id, process_deweather, normalised, poll, unit, location_id, source, output, model)
  # }
  # 
  # results <- dplyr::bind_rows(
  #   results_trend,
  #   results_anomaly_abs,
  #   results_anomaly_rel,
  #   results_anomaly_rel_cf,
  #   results_anomaly_offsetted,
  #   results_counterfactual,
  #   results_counterfactual_nofire,
  #   results_anomaly_yday_abs,
  #   results_anomaly_yday_rel,
  #   results_anomaly_yday_rel_cf,
  #   results_anomaly_yday_offsetted,
  #   results_counterfactual_nofire_yday
  # )  

  return(result)
}


# Group by GADM2 and GADM1
# results_anomaly_gadm1
# process_id replace city->gadm
# post_agg_gadm1 <- function(results, locs){
#   results %>%
#     dplyr::left_join(locs %>% dplyr::select(city, gid_1), by=c("location_id"="city")) %>%
#     dplyr::mutate(process_id=gsub("city","gadm1",process_id)) %>%
#     tidyr::unnest(cols=normalised) %>%
#     dplyr::group_by(process_id, process_deweather, poll, unit, source, gid_1, date, output) %>%
#     dplyr::summarise(value=mean(value, na.rm=T)) %>%
#     tidyr::nest() %>%
#     rename(location_id=gid_1, normalised=data)
# }
# 
# post_agg_gadm2 <- function(results, locs){
#   results %>%
#     left_join(locs %>% dplyr::select(city, gid_1, gid_2), by=c("location_id"="city")) %>%
#     mutate(process_id=gsub("city","gadm2",process_id)) %>%
#     tidyr::unnest(cols=normalised) %>%
#     group_by(process_id, process_deweather, poll, unit, source, gid_2, date, output) %>%
#     summarise(value=mean(value, na.rm=T)) %>%
#     tidyr::nest() %>%
#     rename(location_id=gid_2, normalised=data)
# }

