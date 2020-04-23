# This file defines the post-compute function for models trained with
# rmweather package. The main input is the folder of training results
# Author: Hubert Thieriot

require(dplyr)
require(tidyr)
require(purrr)
require(ggplot2)
require(countrycode)
source('99_plot.R')
source('04_postcompute_utils.R')


# result_folder <- "data/03_train_models/output/20200423_010723_lag0_no2_pm10_deg02_trees600_samples300_normF"

#' Post computing results
#'
#' This function reads train_model results and create charts and tables
#' in the result folder
#' 
#' @param result_folder Folder where the results are stored and charts will be saved
#'
#' @return
#' @export
postcompute_results <- function(result_folder){
  
  # Read data
  result <- list.files(path=result_folder, pattern = "result.RDS", full.names = T) %>%
    map(readRDS) %>% 
    bind_rows() %>%
    ungroup() 
    
  polls <- unique(result$pollutant)
  
  # Prepare data
  result <- postcompute.add_metadata(result)
  result_impact <- postcompute.lockdown_impact(result)
  result_impact <- postcompute.add_gpw(result_impact)
  result_impact_avg <- postcompute.population_weighted(result_impact,
                                                group_by_cols=c('country', 'country_count', 'pollutant'),
                                                value_col=c('diff_ratio','diff'))
  # Add a line for whole Europe
  result_impact_avg <- bind_rows(
    result_impact_avg,
    postcompute.population_weighted(result_impact,
                                  group_by_cols=c('pollutant'),
                                  value_col=c('diff_ratio','diff')) %>%
      left_join(result_impact %>% group_by(pollutant) %>%summarise(country_count=n())) %>%
      mutate(country='Europe')
  )
      
  
  # saveRDS(result_impact, file=file.path(result_folder, 'result_impact.RDS'))
  saveRDS(result_impact %>% 
            dplyr::select(station_id, pollutant, unit, city,country, geometry,
                          movement,avg_observed, avg_predicted,diff, diff_ratio, gpw),
          file=file.path(result_folder, 'result_impact_lite.RDS'))
  
  
  # Plot
  plot.rmweather.impact(result_impact,
                        result_folder=result_folder,
                        min_country_count=10,
                        caption='Source: CREA based on EEA, NASA ISD, Oxford COVID Government Tracker')
  
  plot.rmweather.impact_avg(result_impact_avg,
                        result_folder=result_folder,
                        min_country_count=10,
                        caption='Source: CREA based on EEA, NASA ISD, Oxford COVID Government Tracker')

  
  # Map
  # result_impact
  # 
  # 
  # result_impact %>% na.omit %>% sf::st_as_sf() %>% as('Spatial') -> yoySP
  # gridR = raster::raster(raster::extent(yoySP)*1.1, res=.25, crs=raster::crs(yoySP))
  # yoySP %<>% split(f=.$pollutant)
  # gridR %>% as('SpatialPixels') -> gridSP
  # yoySP %>% 
  #   lapply(function(sp) raster::idw(diff ~ 1, locations=sp, newdata=gridSP)) %>% 
  #   lapply(raster::raster) %>% stack -> stationR# 
  # 
  # gstat::idw(diff ~ 1, locations=yoySP[[1]], newdata=gridSP)
  # 
  # 
  # 
  # # years_befores <- list(seq(2015,2019),
  # #                       seq(2016,2019),
  # #                       seq(2017,2019),
  # #                       seq(2018,2019))
  # # 
  # # 
  # # result_ba <- do.call("bind_rows", lapply(years_befores,calculate_before_after, result=result))
  # # 
  # 
  # # Plot average after lockdown in 2020 over average in same periods years before
  # # With and without weather normalisation
  # 
  # polls <- unique(result_ba$pollutant)
  # for(poll in polls){
  #   poll_result <- result_ba %>%filter(pollutant==poll, !is.na(country), var=='diff_ratio')
  #   (plt <- ggplot(poll_result) + geom_boxplot(aes(x=value, color=normalised, y=country)) +
  #     facet_wrap(~years_before) +
  #     geom_vline(xintercept = 0, linetype='dotted') +
  #     labs(title=paste(toupper(poll),'- Observed vs weather-normalised impacts of lockdown'),
  #          subtitle=paste('Based on', length(unique(result_ba$station_id)),'stations'),
  #          y=NULL, x=NULL) + 
  #     scale_x_continuous(labels = scales::percent, breaks=seq(-1, 1, 0.2), minor_breaks = seq(-1, 1, 0.1), limits=c(-1,1)) +
  #     scale_color_crea_d("dramatic", name=NULL,
  #                          breaks=c('observed','normalised'),
  #                          labels=c('Observed', 'Weather normalised')) +
  #     theme_crea() +
  #     theme(
  #       panel.grid.major.x = element_line(colour = "#DDDDDD"),
  #       panel.grid.minor.x = element_line(colour = "#EEEEEE")
  #     ))
  #     
  #   ggsave(filename = file.path(result_folder, paste0(poll,'_before_after.png')), plot = plt, height=10, width=15, scale=1)
  # }
  # 
  # 
  # 
  # 
  # 
  # locs <- creadb::locations(id=unique(meas_var_pm10$location_id))
  # meas_var_pm10_r <- meas_var_pm10 %>% dplyr::left_join(locs %>% dplyr::select(id, geometry),
  #                                                       by=c('location_id'='id'))
  # meas_var_pm10_r$gpw <- raster::extract(gpw, sf::st_as_sf(meas_var_pm10_r))
  # meas_var_pm10_r$delta <- meas_var_pm10_r$after - meas_var_pm10_r$before
  # meas_var_pm10_r %>% ungroup() %>% filter(!is.na(gpw)) %>%
  #   summarise(delta = weighted.mean(delta,gpw),
  #             ratio = weighted.mean(ratio,gpw))
  # 
  # 
  # # Detect trends
  # add_trend <- function(row, rolling_days=7, max_nas=3){
  #   tryCatch({
  #     # Rolling average
  #     date_cut <- coalesce(min(row$school_workplace, row$movement, na.rm=T), as.POSIXct('2020-03-16'))
  #     date_start <- date_cut - lubridate::years(1)
  #     data <- utils.rolling_average(row$model_fitted[[1]]$normalised, average_by='day', average_width=rolling_days,
  #                                   group_cols=c(), avg_cols= 'value_predict', max_nas=max_nas)
  #     data_before <- data %>% filter(date < date_cut, date >= date_start)
  #     data_after <- data %>% filter(date >= date_cut)
  #     
  #     trend.before <- openair::TheilSen(
  #       data_before,
  #       pollutant = "value_predict",
  #       avg.time = "day",
  #       alpha = 0.05,
  #       plot = F,
  #       silent = F,
  #     )
  #     
  #     trend.after <- openair::TheilSen(
  #       data_after,
  #       pollutant = "value_predict",
  #       avg.time = "day",
  #       alpha = 0.05,
  #       plot = F,
  #       silent = F,
  #     )
  #     
  #     
  #     bind_rows(
  #       trend.before$data$res2 %>% mutate(period='before', date_start=date_start, date_end=date_cut, rolling_days=rolling_days, max_nas=max_nas),
  #       trend.after$data$res2 %>% mutate(period='after', date_start=date_cut, date_end=NA, rolling_days=rolling_days, max_nas=max_nas)) %>%
  #       dplyr::filter(!is.na(p.stars))
  #   }, error=function(err){return(NA)})
  # }
  # 
  # # Very slow...
  # # result <- result %>% purrrlyr::by_row(add_trend, rolling_days=7, max_nas=3, .to ='trend7')
  # # result <- result %>% purrrlyr::by_row(add_trend, rolling_days=30, max_nas=15, .to ='trend30')
  # # result <- result %>% rowwise() %>% mutate(trend=list(rbind(trend7, trend30)))
  # 
  # 
  # ###### Plot results
  plot.rmweather.result_rows(result, rolling_days=7, max_nas=3, w_trend=F,
                             filepath=file.path(result_folder,paste0('results_7d.pdf')))

  plot.rmweather.result_rows(result, rolling_days=30, max_nas=15, w_trend=F,
                             filepath=file.path(result_folder,paste0('results_30d.pdf')))
  
  # plot.rmweather.result_rows(result, rolling_days=7, max_nas=3, w_trend=T,
  #                            filepath=file.path(result_folder,paste0('results_7d_trends.pdf')))
  # 
  # plot.rmweather.result_rows(result, rolling_days=30, max_nas=15, w_trend=T,
  #                            filepath=file.path(result_folder,paste0('results_30d_trends.pdf')))
  
  #   
  #   ###### Average by country
  #   countries <- result %>% rowwise() %>% mutate(normalised=list(model_fitted$normalised)) %>%
  #     select(country, pollutant, unit, normalised) %>% tidyr::unnest(cols=c('normalised')) %>%
  #     group_by(country, pollutant, unit, date) %>%
  #     summarise(value=mean(value_predict, na.rm=T))
  #   
  #   ggplot(countries) + geom_line(aes(x=ate, y=value)) + facet_wrap(~country)
  #   
  #   
  #   ### Plot trends
  #   trends <- result %>% rowwise() %>% tidyr::unnest(cols=c('trend')) %>%
  #     filter(rolling_days==30)
  #   
  #   ggplot(trends) + geom_boxplot(aes(x=country, y=slope, color=period)) +
  #     scale_color_discrete(breaks=c('before','after')) +
  #     labs(y='Trend in NO2 concentration [µg/m3/year]')
  #   scale_color_discrete()
  #   

}
