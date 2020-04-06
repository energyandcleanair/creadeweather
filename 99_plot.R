require(ggplot2)
if (!require(cowplot)) install.packages(c('cowplot')); require(cowplot)
if (!require(ggpubr)) install.packages(c('ggpubr')); require(ggpubr)

source('99_utils.R')
source('99_crea_theme.R')

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
    dplyr::group_by(gadm1_id, pollutant, rsq, rsq_test, mae, mae_test, type) %>%
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
    'Station id'=output_data_row$station_id,
    'Region id'=output_data_row$gadm1_id,
    'Region name'=output_data_row$gadm1_name,
    'Pollutant'=output_data_row$pollutant,
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
  
  figure <- tryCatch({
    p1 <- plot.predicted(output_data_row$meas_weather[[1]],rolling_days) +
      theme(legend.position='none') +
      scale_x_date(limits=c(lubridate::date('2015-01-01'), lubridate::date('2020-01-01'))) +
      ylim(c(0, NA))
    p2 <- plot.predicted(output_data_row$meas_weather[[1]],rolling_days, min_date = '2020-01-01') +
      theme(legend.position='bottom') +
      ylim(c(0, NA))
    p3 <- plot.infos(output_data_row)
    ggarrange(p1, p2, p3, ncol = 3, nrow = 1,  common.legend = TRUE, legend = "bottom", widths=c(2,2,1))
    
  },
  error=function(cond){
    no_data <- "No Data"
    p1 <- ggplot() + theme_void() + ggplot2::annotate("text", x=0, y=0, label=no_data) 
    p2 <- ggplot() + theme_void() + ggplot2::annotate("text", x=0, y=0, label=no_data) 
    p3 <- plot.infos(output_data_row)
    ggarrange(p1, p2, p3, ncol = 3, nrow = 1,  common.legend = TRUE, legend = "bottom", widths=c(2,2,1))
  })
  
  annotate_figure(figure,
                  top=" ",
                  fig.lab = paste(output_data_row$pollutant,
                                  output_data_row$station_id,
                                  output_data_row$gadm1_id,
                                  output_data_row$gadm1_name,
                                  paste0(rolling_days,' days average'),
                                  sep=" - ")
  )
}

plot.output_data <- function(output_data, rolling_days, filepath){
  
  # Arrange / Fill so that pages are homogenous
  filled_output <- output_data %>%
    right_join(tidyr::crossing(output_data %>% distinct(station_id),
                               pollutant=unique(output_data$pollutant))) %>%
    arrange(station_id, pollutant)
  
  figures <- list()
  for(i in seq(1,nrow(filled_output))){
      figures[[i]] <- plot.output_data_row(filled_output[i,], rolling_days)
  }
  
  figure_alls <- ggarrange(plotlist=figures, ncol = 1, nrow = 3)
  ggexport(figure_alls, filename=filepath,  width = 20, height = 20)
}

plot.map_count <- function(data, folder, title, meas_col){
  
  # Plot number of measurements with weather
  map_count_data <- sf::st_as_sf(data) %>%
    mutate_at(meas_col, nrow) %>%
    group_by(station_id, pollutant) %>% 
    mutate_at(meas_col, sum)
  
  map_count <- ggplot(map_count_data) +
    geom_sf(aes_string(fill=meas_col, colour=meas_col),size=0.1) +
    facet_grid(~pollutant) +
    labs(title=title, fill="Count") +
    scale_fill_continuous(na.value="white")
  
  if(!is.null(folder)){
    ggsave(filename=file.path(folder,'map_count.pdf'), map_count)
    ggsave(filename=file.path(folder,'map_count_thumb.png'), map_count, scale=0.7, width=15, height=6, dpi = 120)  
  }
  map_count
}

plot.map_count_per_gadm <- function(data, folder, title, meas_col){
  
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

plot.output_map <- function(output_data, result_folder, timestamp_str, meas_col, title, scale=NULL, labs=NULL){
  gadm1_sf <- sf::st_read('data/00_init/output/gadm1.geojson')
  map_data <- st_as_sf(data)
 
  map_ <- ggplot(map_data) + geom_sf(data=gadm1_sf) + 
    geom_sf(aes_string(fill=meas_col),size=0.1) +
    facet_grid(~pollutant) +
    labs(title=title)
  
  map_ <- map_ + if(is.null(scale)) scale_fill_continuous(na.value="white") else scale
  map_ <- map_ + if(is.null(labs)) labs(fill="") else labs
  ggsave(file.path(result_folder,paste0(timestamp_str,'_map_',meas_col,'.pdf')), plot=map_)
  map_
  
}

plot.output_map_gadm <- function(output_data, result_folder, timestamp_str, meas_col, title, scale=NULL, labs=NULL){
  gadm1_sf <- sf::st_read('data/00_init/output/gadm1.geojson')
  gadm1_data <- gadm1_sf %>% dplyr::right_join(
    output_data %>%
      dplyr::select_at(c('gadm1_id', 'pollutant', meas_col)) %>%
      right_join(tidyr::crossing(output_data %>% distinct(gadm1_id, gadm1_name),
                                 pollutant=unique(output_data$pollutant)))
    )
  
  map_ <- ggplot(gadm1_data) +
    geom_sf(aes_string(fill=meas_col),size=0.1) +
    facet_grid(~pollutant) +
    labs(title=title)
  
  map_ <- map_ + if(is.null(scale)) scale_fill_continuous(na.value="white") else scale
  map_ <- map_ + if(is.null(labs)) labs(fill="") else labs
  ggsave(file.path(result_folder,paste0(timestamp_str,'_map_',meas_col,'.pdf')), plot=map_)
  map_
  
}
