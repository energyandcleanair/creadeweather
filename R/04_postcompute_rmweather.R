# This file defines the post-compute function for models trained with
# rmweather package. The main input is the folder of training results
# Author: Hubert Thieriot


# result_folder <- "data/03_train_models/output/20200423_041149_lag1_no2_pm10_deg02_trees600_samples300_normF"

#' Post computing results
#'
#' This function reads train_model results and create charts and tables
#' in the result folder
#' 
#' @param result_folder Folder where the results are stored and charts will be saved
#'
#' @return
#' @export
postcompute_results_rmweather <- function(result_folder){
  
  # Read data
  result <- list.files(path=result_folder, pattern = "result.RDS", full.names = T) %>%
    purrr::map(readRDS) %>% 
    dplyr::bind_rows() %>%
    dplyr::ungroup() 
  
  polls <- unique(result$pollutant)
  
  # Prepare data
  result <- postcompute.add_metadata(result)
  result <- postcompute.quality_filter(result,
                                       rsq.threshold = 0.2,
                                       mse.thresholds = list("PM10"=500, "NO2"=500))
  result_impact <- postcompute.lockdown_impact(result)
  result_impact <- postcompute.add_gpw(result_impact)
  result_impact_avg <- postcompute.population_weighted(result_impact,
                                                       group_by_cols=c('country', 'country_count', 'pollutant'),
                                                       value_col=c('diff_ratio','diff'))
  # Add a line for whole Europe
  result_impact_avg <- dplyr::bind_rows(
    result_impact_avg,
    postcompute.population_weighted(result_impact,
                                    group_by_cols=c('pollutant'),
                                    value_col=c('diff_ratio','diff')) %>%
      dplyr::left_join(result_impact %>% dplyr::group_by(pollutant) %>% dplyr::summarise(country_count=n())) %>%
      dplyr::mutate(country='Europe')
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
                        caption='Source: CREA based on EEA, NOAA ISD, UNCAR, Oxford COVID Government Response Tracker')
  
  plot.rmweather.impact_avg(result_impact_avg,
                            result_folder=result_folder,
                            min_country_count=10,
                            caption='Source: CREA based on EEA, NOAA ISD, UNCAR, Oxford COVID Government Response Tracker')
  
  plot.rmweather.impact_avg(result_impact_avg,
                            result_folder=result_folder,
                            min_country_count=10,
                            caption='Source: CREA based on EEA, NOAA ISD, UNCAR, Oxford COVID Government Response Tracker')
  
  
  plot.rmweather.qc_plot(result,
                      result_folder=result_folder)
  
  ###### Plot detailed results
  plot.rmweather.result_rows(result, rolling_days=7, max_nas=3, normalised=T, w_trend=F,
                             filepath=file.path(result_folder,paste0('results_7d_normalised.pdf')))
  
  plot.rmweather.result_rows(result, rolling_days=7, max_nas=3, normalised=F, w_trend=F,
                             filepath=file.path(result_folder,paste0('results_7d.pdf')))
  
  plot.rmweather.result_rows(result, rolling_days=30, max_nas=15, normalised=T, w_trend=F,
                             filepath=file.path(result_folder,paste0('results_30d_normalised.pdf')))
  
  plot.rmweather.result_rows(result, rolling_days=30, max_nas=15, normalised=F, w_trend=F,
                             filepath=file.path(result_folder,paste0('results_30d.pdf')))
  
}
