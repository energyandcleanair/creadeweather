require(ggplot2)
if (!require(cowplot)) install.packages(c('cowplot')); require(cowplot)
if (!require(GADMTools)) install.packages(c('GADMTools')); require(GADMTools)

source('99_utils.R')

plot.tools.gather_predicted <- function(meas_weather){
  meas_weather %>%
    dplyr::select(date, value, predicted) %>%
    tidyr::gather('type', 'value', -c(date))
}


plot.tools.roll_plot_gathered <- function(raw){
  raw %>%
    dplyr::mutate(date=lubridate::floor_date(date, unit = 'day')) %>%
    dplyr::group_by(date, type) %>%
    dplyr::summarise(value=mean(value, na.rm = T)) %>% dplyr::ungroup() %>%
    dplyr::group_by(gadm1_id, AirPollutant, rsq, rsq_test, mae, mae_test, type) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(value=zoo::rollapply(value, width=n_days,
                                       FUN=function(x) mean(x, na.rm=TRUE), align='right',fill=NA)) %>%
    dplyr::ungroup()
}

plot.predicted <- function(meas_weather, rolling_days, min_date=NULL){
  
  if(!is.null(min_date)){
    meas_weather <- meas_weather %>% filter(date>=lubridate::date(min_date) - lubridate::days(rolling_days))  
  }
  
  meas_weather_gathered <- plot.tools.gather_predicted(meas_weather)
  meas_weather_gathered_rolled <- utils.rolling_average(meas_weather_gathered,
                                                        average_by='day',
                                                        average_width = rolling_days,
                                                        group_cols=c('type'),
                                                        avg_cols=c('value'))
  
  plot <- ggplot(meas_weather_gathered_rolled, aes(x=date,y=value,colour=type)) +
    geom_line()
  
  if(!is.null(min_date)){
    plot <- plot + xlim(lubridate::date(min_date),NA)
  }
  return(plot)
}

plot.infos <- function(output_data_row){
  
  infos_list <- list(
    'Region id'=output_data_row$gadm1_id,
    'Region name'=output_data_row$gadm1_name,
    'Pollutant'=output_data_row$AirPollutant,
    'Model'=output_data_row$model_name,
    'r2 training'=round(output_data_row$rsq,2),
    'r2 validation'=round(output_data_row$rsq_test,2),
    'mae training'=round(output_data_row$mae,2),
    'mae validation'=round(output_data_row$mae_test,2)
  )
  infos_tbl <- tibble('Parameter / Result'=names(infos_list), 'Value'=as.character(infos_list))
  ggtexttable(infos_tbl, rows = NULL, 
                          theme = ttheme("lCyan"))
}

plot.output_data_row <- function(output_data_row, rolling_days){
  
  tryCatch({
    p1 <- plot.predicted(output_data_row$meas_weather[[1]],rolling_days) +
      theme(legend.position='none') +
      scale_x_date(limits=c(lubridate::date('2015-01-01'), lubridate::date('2020-01-01')))
    p2 <- plot.predicted(output_data_row$meas_weather[[1]],rolling_days, min_date = '2020-01-01') +
      theme(legend.position='bottom')
    p3 <- plot.infos(output_data_row)
    figure <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1,  common.legend = TRUE, legend = "bottom", widths=c(2,2,1))
    
  },
  error=function(cond){
    no_data <- "No Data"
    p1 <- ggplot() + theme_void() + ggplot2::annotate("text", label=no_data) 
    p2 <- ggplot() + theme_void() + ggplot2::annotate("text", label=no_data) 
    p3 <- plot.infos(output_data_row)
    figure <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1,  common.legend = TRUE, legend = "bottom", widths=c(2,2,1))
  })
  
  annotate_figure(figure,
                  top=" ",
                  fig.lab = paste(output_data_row$AirPollutant,
                                  output_data_row$gadm1_id,
                                  output_data_row$gadm1_name,
                                  paste0(rolling_days,' days average'),
                                  sep=" - ")
  )
}

plot.output_data <- function(output_data, rolling_days, filepath){
  
  # Arrange / Fill so that pages are homogenous
  filled_output <- output_data %>%
    right_join(tidyr::crossing(output_data %>% distinct(gadm1_id, gadm1_name),
                               AirPollutant=unique(output_data$AirPollutant))) %>%
    arrange(gadm1_id, AirPollutant)
  
  figures <- list()
  for(i in seq(1,nrow(filled_output))){
      figures[[i]] <- plot.output_data_row(filled_output[i,], rolling_days)
  }
  
  figure_alls <- ggarrange(plotlist=figures, ncol = 1, nrow = 3)
  ggexport(figure_alls, filename=filepath,  width = 20, height = 20)
}

plot.map_count <- function(data, folder, title, meas_col){
  
  # Plot number of measurements with weather
  gadm1_sf <- sf::st_read('data/00_init/output/gadm1.geojson')
  map_count_data <- st_as_sf(gadm1_sf %>% 
                               tidyr::crossing(pollutant=unique(data$pollutant))) %>%
    left_join(
      data %>%
        rowwise() %>% 
        mutate_at(meas_col, nrow)
      ) %>%
    group_by(gadm1_id, pollutant) %>% 
    mutate_at(meas_col, sum)
    
  map_count <- ggplot(map_count_data) +
    geom_sf(aes_string(fill=meas_col),size=0.1) +
    facet_grid(~pollutant) +
    labs(title=title, fill="Count") +
    scale_fill_continuous(na.value="white")
  
  if(!is.null(folder)){
    ggsave(filename=file.path(folder,'map_count.pdf'), map_count)
    ggsave(filename=file.path(folder,'map_count_thumb.png'), map_count, scale=0.7, width=15, height=6, dpi = 120)  
  }
  map_count
}

plot.output_map <- function(output_data, result_folder, timestamp_str){
  gadm1_sf <- sf::st_read('data/00_init/output/gadm1.geojson')
  polls <- uniaue(Ai)
  gadm1_data <- gadm1_sf %>% dplyr::right_join(
    output_data %>%
      dplyr::select(gadm1_id, AirPollutant, rmse, rmse_test, mae, mae_test, rsq, rsq_test) %>%
      right_join(tidyr::crossing(output_data %>% distinct(gadm1_id, gadm1_name),
                                 AirPollutant=unique(output_data$AirPollutant)))
    )
  
  map_r2_test <- ggplot(gadm1_data) +
    geom_sf(aes(fill=rsq_test),size=0.1) +
    facet_grid(~AirPollutant) +
    labs(title=expression(paste(R^{2}, "validation"))) +
    scale_fill_continuous(na.value="white")
  
  ggsave(file.path(result_folder,paste0(timestamp_str,'_map_rsq_test.pdf')))
  
}
