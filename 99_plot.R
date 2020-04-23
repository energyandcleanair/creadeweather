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
    dplyr::group_by(station_id, pollutant, rsq, rsq_test, mae, mae_test, mrae_test, mrae_test, type) %>%
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
    'mae validation'=round(output_data_row$mae_test,2),
    'mase training'=round(output_data_row$mase,2),
    'mase validation'=round(output_data_row$mase_test,2),
    'mrae training'=round(output_data_row$mrae,2),
    'mrae validation'=round(output_data_row$mrae_test,2),
    'me training'=round(output_data_row$me,2),
    'me validation'=round(output_data_row$me_test,2),
    'mpe training'=round(output_data_row$mpe,2),
    'mpe validation'=round(output_data_row$mpe_test,2)
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
  
  gadm1_sf <- sf::st_read('data/00_init/output/gadm1.geojson')
  
  # Plot number of measurements with weather
  map_count_data <- sf::st_as_sf(data) %>%
    mutate_at(meas_col, nrow) %>%
    group_by(station_id, pollutant) %>% 
    mutate_at(meas_col, sum)
  
  map_count <- ggplot(map_count_data) + geom_sf(data=gadm1_sf, alpha=0.1) +
    geom_sf(data=map_count_data, aes_string(fill=meas_col, colour=meas_col), size=0.5) +
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

plot.output_map <- function(data, result_folder, timestamp_str, meas_col, title, scale=NULL, labs=NULL){
  gadm1_sf <- sf::st_read('data/00_init/output/gadm1.geojson')
  map_data <- st_as_sf(data, crs=4326)
 
  map_ <- ggplot(map_data) + geom_sf(data=gadm1_sf, colour='#DDDDDD', alpha=0.2) + 
    geom_sf(aes_string(colour=meas_col),size=1) +
    facet_grid(~pollutant) +
    labs(title=title) + theme_crea()
  
  map_ <- map_ + if(is.null(scale)) scale_fill_continuous(na.value="white") else scale
  map_ <- map_ + if(is.null(labs)) labs(fill="") else labs
  ggsave(file.path(result_folder,paste0(timestamp_str,'_map_',meas_col,'.pdf')),
         plot=map_,
         scale=2)
  map_
  
}

plot.output_map_criteria <- function(data, result_folder, timestamp_str, meas_col, criteria_col, title, scale=NULL, labs=NULL){
  gadm1_sf <- sf::st_read('data/00_init/output/gadm1.geojson')
  map_data <- st_as_sf(data)
  
  map_ <- ggplot(map_data) + geom_sf(data=gadm1_sf, colour='#DDDDDD', alpha=0.2) + 
    geom_sf(aes_string(fill=meas_col, colour=criteria_col),size=1) +
    facet_grid(~pollutant) +
    labs(title=title) + theme_crea()
  
  map_ <- map_ + if(is.null(scale)) scale_fill_continuous(na.value="white") else scale
  map_ <- map_ + if(is.null(labs)) labs(fill="") else labs
  ggsave(file.path(result_folder,paste0(timestamp_str,'_map_',meas_col,'_criteria.pdf')),
         plot=map_,
         scale=3)
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
    labs(title=title) + theme_crea()
  
  map_ <- map_ + if(is.null(scale)) scale_fill_continuous(na.value="white") else scale
  map_ <- map_ + if(is.null(labs)) labs(fill="") else labs
  ggsave(file.path(result_folder,paste0(timestamp_str,'_map_',meas_col,'.pdf')), plot=map_)
  map_
  
}

plot.output_result_quality <- function(output_data, result_folder, timestamp_str){
  p1 <- ggplot(output_data %>% dplyr::select(pollutant, rsq_test, rsq) %>% tidyr::gather("key","value",-pollutant)) + 
    geom_histogram(aes(x=value, fill=key, colour=key), alpha=0.2) + facet_grid(~pollutant) + theme_crea()
  
  p2 <- ggplot(output_data %>% dplyr::select(pollutant, mae_test, mae) %>% tidyr::gather("key","value",-pollutant)) + 
    geom_histogram(aes(x=value, fill=key, colour=key), alpha=0.2) + facet_grid(~pollutant) + theme_crea()
  
  ggsave(file.path(result_folder,paste0(timestamp_str,'_quality.pdf')),
         plot=  ggarrange(p1, p2, ncol = 2, nrow = 1,  common.legend = FALSE, legend = "bottom", widths=c(2,2)))
}

#------------------------------
# Plot for rmweather results
#------------------------------
plot.rmweather.normalized <- function(row, rolling_days, max_nas=NULL){
  
  lubridate::year(row$movement) <- 0
  lubridate::year(row$school_workplace) <- 0

  model_fitted <- row$model_fitted[[1]]
  data <- model_fitted$normalised
  data <- utils.rolling_average(data, average_by = 'day', average_width = rolling_days,
                                group_cols = c(), avg_cols='value_predict', max_nas=max_nas)
  data$year <- factor(lubridate::year(data$date))
  data$date_in_year <- data$date
  lubridate::year(data$date_in_year) <- 0
  ggplot(data) + geom_line(aes(x=date_in_year, y=value_predict, color=year, size=year)) +
    labs(y='Weather normalized concentration', x=NULL, size='Year', colour="Year") +
    scale_size_manual(values=c(1.1,0.6,0.6,0.6,0.6,0.6), breaks = seq(2020,2015)) +
    # scale_color_manual(values=c('#8c510a','#bf812d','#dfc27d','#01665e','#35978f','#80cdc1'), breaks = seq(2020,2015)) +
    scale_color_discrete(breaks = seq(2020,2015)) +
    theme_crea() +
    theme(legend.position="bottom") +
    geom_vline(data=row, aes(xintercept=as.POSIXct(movement), linetype='1'), colour=pal_crea['Turquoise']) +
    geom_vline(data=row, aes(xintercept=as.POSIXct(school_workplace), linetype='2'), colour=pal_crea['Turquoise'])+
    scale_linetype_manual(name = "", labels=c('Lockdown (movement)', 'Lockdown (school, work)'), values=c('dashed','dotted')) +
    guides(fill = guide_legend(nrow = 2, title.position = "left"),
           size= guide_legend(nrow = 2, title.position = "left"),
           linetype = guide_legend(nrow = 2, title.position = "left"))
    
    
}


plot.rmweather.normalized_w_trend <- function(row, rolling_days, max_nas=NULL){
  rolling_days_ <- rolling_days
  model_fitted <- row$model_fitted[[1]]
  data <- model_fitted$normalised
  data <- utils.rolling_average(data, average_by = 'day', average_width = rolling_days,
                                group_cols = c(), avg_cols='value_predict', max_nas=max_nas)

  date_end <- max(data$date)
  trend <- row$trend[[1]] %>% filter(rolling_days==rolling_days_) %>% tidyr::replace_na(list(date_end=max(data$date))) %>%
    mutate(title=paste(period,p.stars))
  
  min_date <- coalesce(min(trend$date_start), as.POSIXct('2019-01-01'))
  ggplot(data %>% filter(date>=min_date)) + geom_line(aes(x=date, y=value_predict)) +
    labs(y='Weather normalized concentration', x=NULL, colour="Trend") +
    scale_size_manual(values=c(1.1,0.6,0.6,0.6,0.6,0.6), breaks = seq(2020,2015)) +
    # scale_color_manual(values=c('#8c510a','#bf812d','#dfc27d','#01665e','#35978f','#80cdc1'), breaks = seq(2020,2015)) +
    scale_color_discrete() +
    theme_crea() +
    theme(legend.position="bottom") +
    geom_vline(data=row, aes(xintercept=as.POSIXct(movement), linetype='1'), colour=pal_crea['Turquoise']) +
    geom_vline(data=row, aes(xintercept=as.POSIXct(school_workplace), linetype='2'), colour=pal_crea['Turquoise'])+
    scale_linetype_manual(name = "", labels=c('Lockdown (movement)', 'Lockdown (school, work)'), values=c('dashed','dotted')) +
    guides(fill = guide_legend(nrow = 2, title.position = "left"),
           color= guide_legend(nrow = 2, title.position = "left"),
           linetype = guide_legend(nrow = 2, title.position = "left")) +
    geom_segment(data=trend, aes(x=date_start,
                     y=a + b*interval(as.POSIXct('1970-01-01'),date_start)%/%lubridate::days(1),
                     xend=date_end,
                     yend=a + b*interval(as.POSIXct('1970-01-01'),date_end)%/%lubridate::days(1),
                     color=title),
                 size=1) +
    geom_segment(data=trend, aes(x=date_start,
                                 y=upper.a + upper.b*interval(as.POSIXct('1970-01-01'),date_start)%/%lubridate::days(1),
                                 xend=date_end,
                                 yend=upper.a + upper.b*interval(as.POSIXct('1970-01-01'),date_end)%/%lubridate::days(1),
                                 color=title),
                 size=0.3) + 
    geom_segment(data=trend, aes(x=date_start,
                                 y=lower.a + lower.b*interval(as.POSIXct('1970-01-01'),date_start)%/%lubridate::days(1),
                                 xend=date_end,
                                 yend=lower.a + lower.b*interval(as.POSIXct('1970-01-01'),date_end)%/%lubridate::days(1),
                                 color=title),
                 size=0.3)
}

plot.rmweather.importance <- function(model_fitted){
  
  data_list <- model_fitted$model$variable.importance
  data <- tibble(variable=names(data_list), importance=data_list)
  # Group all lag variables into 1
  data$variable <- gsub("_\\d","",data$variable)

  ggplot(data) + geom_boxplot(aes(y=reorder(variable, importance, FUN = median),x=importance)) +
    labs(y='Variable (all lags combined)') + #theme_crea() +
    theme(legend.position="bottom")
}


plot.rmweather.infos <- function(row){
  infos_list <- list(
    'Station id'=row$station_id,
    'Country id'=row$gadm0_id,
    'Region id'=row$gadm1_id,
    'Region'=row$gadm1_name,
    'Pollutant'=row$pollutant,
    'Trees'=row$model_fitted[[1]]$model$num.trees,
    'R2'=round(row$model_fitted[[1]]$model$r.squared,2),
    'Prediction error'=round(row$model_fitted[[1]]$model$prediction.error,2)
  )
  infos_tbl <- tibble('Parameter / Result'=names(infos_list), 'Value'=as.character(infos_list))
  ggtexttable(infos_tbl, rows = NULL, 
              theme = ttheme("lCyan"))
}

plot.rmweather.result_row <- function(row, rolling_days, max_nas=NULL, w_trend=F){
  
  figure <- tryCatch({
    p1 <- plot.rmweather.normalized(row,rolling_days, max_nas=max_nas)
    if(w_trend){
      p2 <- plot.rmweather.normalized_w_trend(row,rolling_days, max_nas=max_nas)
      # p2 <- tryCatch({},
      #                error=function(cond){
      #                  warning(paste("Failed to build trend:", cond))
      #                  ggplot() + theme_void() + ggplot2::annotate("text", x=0, y=0, label=no_data)})
    }else{
      p2 <- p1 +
        scale_x_datetime(limits=c(as.POSIXct('0-01-01'), as.POSIXct('0-05-01')))
    }
    
    p3 <- plot.rmweather.importance(row$model_fitted[[1]])
    p4 <- plot.rmweather.infos(row)
    ggarrange(p1, p2, p3, p4, ncol = 4, nrow = 1,  common.legend = F, legend = "bottom", widths=c(2,2,2,1))
  },
  error=function(cond){
    no_data <- "No Data"
    p1 <- ggplot() + theme_void() + ggplot2::annotate("text", x=0, y=0, label=no_data) 
    p2 <- ggplot() + theme_void() + ggplot2::annotate("text", x=0, y=0, label=no_data) 
    p3 <- ggplot() + theme_void() + ggplot2::annotate("text", x=0, y=0, label=no_data) 
    p4 <- plot.rmweather.infos(row)
    ggarrange(p1, p2, p3, p4, ncol = 4, nrow = 1,  common.legend = TRUE, legend = "bottom", widths=c(2,2,2,1))
  })
  
  annotate_figure(figure,
                  top=" ",
                  fig.lab = paste(row$pollutant,
                                  row$station_id,
                                  row$gadm1_id,
                                  paste0(rolling_days,' days average'),
                                  sep=" - ")
  )
}


plot.rmweather.result_rows <- function(rows, rolling_days, filepath, max_nas=NULL, w_trend=F){
  
  # Arrange / Fill so that pages are homogenous
  rows_filled <- rows %>%
    right_join(tidyr::crossing(rows %>% distinct(station_id),
                               pollutant=unique(rows$pollutant))) %>%
    arrange(station_id, pollutant)
  
  figures <- list()
  for(i in seq(1,nrow(rows_filled))){
    figures[[i]] <- plot.rmweather.result_row(rows_filled[i,], rolling_days, max_nas=max_nas, w_trend=w_trend)
  }
  
  figure_alls <- ggarrange(plotlist=figures, ncol = 1, nrow = 3)
  ggexport(figure_alls, filename=filepath,  width = 20, height = 20)
}

plot.rmweather.before_after <- function(result, years_before, poll, nstations){
  
  average_after <- function(observations, lockdown_date, value_col='value'){
    as.double(observations %>% filter(date >= lockdown_date) %>%
                summarise_at(value_col, mean, na.rm=T))
  }
  
  average_before <- function(observations, lockdown_date, years, value_col='value'){
    max_date <- max(observations$date)
    as.double(observations %>% filter(lubridate::year(date) %in% years,
                                      lubridate::yday(date) >= lubridate::yday(lockdown_date),
                                      lubridate::yday(date) <= lubridate::yday(max_date)
    ) %>%
      summarise_at(value_col, mean, na.rm=T))
  }
  
  result <- result %>% rowwise() %>%
    mutate(after=average_after(model_fitted$observations, movement),
           before=average_before(model_fitted$observations, movement, years_before),
           after_normalised=average_after(model_fitted$normalised, movement, value_col='value_predict'),
           before_normalised=average_before(model_fitted$normalised, movement, years_before, value_col='value_predict'))
  
  result <- result %>% mutate(var_pct=(after-before)/before,
                              var_pct_normalised=(after_normalised-before_normalised)/before_normalised,
                              )
  
  plot_data <- result %>% select(country, pollutant, var_pct, var_pct_normalised) %>%
    tidyr::gather('type', 'average', -c(country, pollutant))
  
  # ggplot(plot_data) +
  #   geom_boxplot(aes(x=country, y=average, colour=factor(period, levels =c('before','after','before_normalised','after_normalised'))))
  ggplot(plot_data) +
    geom_boxplot(aes(y=country, x=average, colour=type)) +
    geom_vline(xintercept = 0, linetype='dotted') +
    labs(title=paste(toupper(poll),'- Observed vs weather-normalised impacts of lockdown'),
         subtitle=paste('Based on', nstations,'stations'),
         y='Variation Before / After') + 
    scale_x_continuous(labels = scales::percent) + 
    scale_color_crea_d("dramatic", breaks=c('var_pct','var_pct_normalised'), labels=c('Observed', 'Weather normalised')) +
    theme_crea()
}

plot.rmweather.impact <- function(result_impact, min_country_count=NULL,
                                  result_folder=NULL, polls=NULL,
                                  caption=NULL, ...){
  
  polls <- if(is.null(polls)) unique(result_impact$pollutant) else polls
  result_impact_f <- result_impact %>%
    filter(pollutant %in% polls) %>%
    filter(is.null(min_country_count) || country_count>=min_country_count)
  
  plt <- ggplot(result_impact_f) +
    geom_boxplot(aes(x=diff_ratio,
                     y=if(length(polls)>1) country else paste0(country,' (',country_count,')'))) +
    geom_vline(xintercept = 0, linetype='dotted') +
    labs(subtitle='Weather-corrected values',
         y=NULL, x=NULL, caption=caption) + 
    scale_x_continuous(labels = scales::percent, breaks=seq(-1, 1, 0.2), minor_breaks = seq(-1, 1, 0.1), limits=c(-1,1)) +
    scale_color_crea_d("dramatic", name=NULL,
                       breaks=c('observed','normalised'),
                       labels=c('Observed', 'Weather normalised')) +
    theme_crea() +
    theme(
      panel.grid.major.x = element_line(colour = "#DDDDDD"),
      panel.grid.minor.x = element_line(colour = "#EEEEEE")
    )
  
  if(length(polls)>1){
    plt <- plt + facet_wrap(~pollutant) + labs(title=paste('Lockdown impact on air pollutant levels'))
  }else{
    plt <- plt + labs(title=paste('Lockdown impact on', toupper(polls), 'levels'))
  }
  
  if(!is.null(result_folder)){
    ggsave(filename=file.path(result_folder, paste0(paste0(polls,collapse='_'),'_lockdown_impact.png')),
           plot=plt, height=10, width=15, scale=1, ...)  
  }
  return(plt)
}

plot.rmweather.impact_avg <- function(result_impact_avg, min_country_count=NULL,
                                  result_folder=NULL, polls=NULL,
                                  caption=NULL, ...){
  
  polls <- if(is.null(polls)) unique(result_impact_avg$pollutant) else polls
  result_impact_avg_f <- result_impact_avg %>%
    filter(pollutant %in% polls) %>%
    filter(is.null(min_country_count) || country_count>=min_country_count)
  
  plt <- ggplot(result_impact_avg_f, aes(x=diff_ratio,
                                         y=if(length(polls)>1) country else paste0(country,' (',country_count,')'),
                                         fill=(country!='Europe'))) +
    geom_col(position = 'dodge') +
    geom_text(aes(label = scales::percent(diff_ratio, accuracy=1), hjust=(diff_ratio<0)),  position = position_dodge(0.9), ) +
    geom_vline(xintercept = 0, linetype='dotted') +
    labs(subtitle='Weather-corrected and population-weighted values',
         y=NULL, x=NULL, caption=caption) + 
    scale_x_continuous(labels = scales::percent, breaks=seq(-1, 1, 0.2), minor_breaks = seq(-1, 1, 0.1), limits=c(-1,1)) +
    scale_fill_crea_d("dramatic", name=NULL,
                       breaks=c('observed','normalised'),
                       labels=c('Observed', 'Weather normalised')) +
    theme_crea() +
    theme(
      panel.grid.major.x = element_line(colour = "#DDDDDD"),
      panel.grid.minor.x = element_line(colour = "#EEEEEE")
    )
  
  if(length(polls)>1){
    plt <- plt + facet_wrap(~pollutant) + labs(title=paste('Lockdown impact on air pollutant levels'))
  }else{
    plt <- plt + labs(title=paste('Lockdown impact on', toupper(polls), 'levels'))
  }
  
  if(!is.null(result_folder)){
    ggsave(filename=file.path(result_folder, paste0(paste0(polls,collapse='_'),'_lockdown_impact_avg.png')),
           plot=plt, height=10, width=15, scale=1, ...)  
  }
  return(plt)
}
