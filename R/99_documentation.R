# Generating the various charts used for documentaion (e.g. for NCAP page)


document_anomaly <- function(){
  
  # Download meas
  library(tidyverse)
  library(ggrepel)
  library(glue)


  location_id <- rcrea::cities(name=c("Delhi", "Mumbai", "Kolkata", "Varanasi",
                                      "Lucknow", "Amritsar", "Noida"))$id
  month_yoy <- "2024-06-01"
  weather_file <- "tmp/weather_documentation.RDS"  
  weather_file_fire <- "tmp/weather_documentation_fire.RDS"  
  file.remove(weather_file)
  file.remove(weather_file_fire)

  deweathered_trend <- creadeweather::deweather(
    location_id = location_id,
    deweather_process_id = "default_trend",
    poll="pm25",
    source="cpcb",
    upload_results = F,
    save_weather_filename = weather_file,
    read_weather_filename = weather_file,
    weather_update_era5 = F
  )
  
  deweathered_trend_fire <- creadeweather::deweather(
    location_id = "delhi_ind.25_1_in",
    deweather_process_id = "default_trend_fire_96h",
    poll="pm25",
    source="cpcb",
    upload_results = F,
    save_weather_filename = weather_file_fire,
    read_weather_filename = weather_file_fire,
    weather_update_era5 = F,
    use_weather_cache = T
  )
  

  deweathered <- creadeweather::deweather(
    location_id = location_id,
    deweather_process_id = "default_anomaly_2018_2099",
    poll="pm25",
    source="cpcb",
    upload_results = F,
    save_weather_filename = weather_file,
    read_weather_filename = weather_file,
    weather_update_era5 = F
  )
  

  deweathered_yoy <- creadeweather::deweather_yoy(
    location_id = location_id,
    months = month_yoy,
    deweather_process_id = "default_anomaly_2018_2099",
    poll="pm25",
    source="cpcb",
    upload_results = F,
    keep_nonyoy_results = T,
    save_weather_filename = weather_file,
    read_weather_filename = weather_file,
    weather_update_era5 = F
  )
  
  deweathered_yoy_fire <- creadeweather::deweather_yoy(
    location_id = location_id,
    months = month_yoy,
    deweather_process_id = "default_anomaly_2018_2099_fire",
    poll="pm25",
    source="cpcb",
    upload_results = F,
    keep_nonyoy_results = T,
    save_weather_filename = weather_file_fire,
    read_weather_filename = weather_file_fire,
    weather_update_era5 = F
  )

  # Plot trend
  deweathered_trend_fire %>%
    filter(source=="cpcb") %>%
    tidyr::unnest(result) %>%
    filter(date >= "2018-01-01") %>%
    filter(variable %in% c("trend", "observed")) %>%
    select(location_id, date, variable, value) %>%
    rcrea::utils.running_average(30, min_values = 20) %>%
    ggplot(aes(x=date, y=value)) +
    geom_hline(yintercept=0, color="grey80") +
    geom_line(aes(color=variable)) +
    # geom_text_repel(
    #   data = deweathered$result[[1]] %>% filter(date==max(date)),
    #   aes(label=variable, color=variable),
    #   nudge_x = 1,
    #   nudge_y = 1,
    #   size=3
    # ) +
    facet_wrap(~variable, ncol=1) +
    rcrea::theme_crea() +
    theme(strip.placement = "top") +
    labs(
      title="PM2.5 deweathered trend in Delhi and Mumbai",
      subtitle="30-day running average",
      y="PM2.5 (µg/m³)",
      x=NULL
    ) +
    scale_color_manual(values=c("trend"=rcrea::pal_crea[["Dark.red"]], "observed"=rcrea::pal_crea[["Blue"]])) +
    facet_wrap(~location_id) +
    rcrea::scale_y_crea_zero()

  ggsave("doc/figures/deweathering_ts_trend.png", width=10, height=6, scale=1.5)



  # Plot with cut dates
  deweathered_yoy %>%
    filter(source=="cpcb") %>%
    select(location_id, result) %>%
    tidyr::unnest(result) %>%
    rcrea::utils.running_average(30,  min_values=20) %>%
    filter(variable %in% c("anomaly", "observed", "predicted")) -> plt_data
    
  ggplot(plt_data) +
    geom_hline(yintercept=0, color="grey80") +
    geom_line(data=plt_data, aes(x=date, y=value, color=variable)) +
    geom_rect(
      data=function(x) filter(x, date==max(date), variable %in% c("anomaly", "observed")),
      aes(xmin=as.Date(month_yoy), xmax=as.Date(month_yoy) + lubridate::days_in_month(month_yoy),
              ymin=-Inf, ymax=Inf, fill="Excluded from training"),
              alpha=0.2) +
    # Add same one a year before
    geom_rect(
      data=function(x) filter(x, date==max(date), variable %in% c("anomaly", "observed")),
      aes(xmin=as.Date(month_yoy) - lubridate::years(1), xmax=as.Date(month_yoy) + lubridate::days_in_month(month_yoy) - lubridate::years(1),
      ymin=-Inf, ymax=Inf, fill="Excluded from training"),
      alpha=0.2) +
    # scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
    # geom_text_repel(
    #   data = deweathered_yoy$result[[1]] %>% filter(date==max(date)),
    #   aes(x=date, y=value, label=variable, color=variable),
    #   nudge_x = 1,
    #   nudge_y = 1,
    #   size=3
    # ) +
    facet_wrap(case_when(variable=="anomaly"~ "Anomaly", T ~ "Observed and Predicted") ~ location_id, ncol=2) +
    rcrea::theme_crea() +
    theme(strip.placement = "top") +
    labs(
      title="Weather-based prediction of PM2.5 in Delhi",
      subtitle="Anomaly = Observed - Predicted | 30-day running average",
      y="PM2.5 (µg/m³)",
      x=NULL,
      fill=NULL,
      color=NULL
    ) +
    rcrea::scale_color_crea_d()


  ggsave("doc/figures/deweathering_ts_yoy.png", width=10, height=6, scale=1.5)


  # Plot yoy change
  deweathered_yoy %>%
    filter(source=="cpcb") %>%
   select(location_id, result) %>%
    tidyr::unnest(result) %>%
    filter(grepl("yoy.*_rel", variable)) %>%    
    mutate(variable_str=case_when(
      grepl("total", variable) ~ "Observed change yoy",
      grepl("weather", variable) ~ "Influence of weather",
      grepl("emission", variable) ~ "Influence of emissions"
    )) %>%
    ggplot(aes(y=value, x=1)) +
    # limit width of column
    geom_col(data=function(x) filter(x, variable != "yoy_total_rel"),
              aes(fill=variable_str),
              width=0.5) +
    geom_point(data=function(x) filter(x, variable=="yoy_total_rel"), size=4) +
    geom_text(
      # data=function(x) filter(x, variable=="yoy_total_rel"),
      aes(label=glue("{variable_str}: {paste0(ifelse(value>0,'+',''), round(value, 2)*100,'%')}"),
          y=value,
          vjust=ifelse(value>0,-1,1)*1.5),
      # nudge_x = 0.0,
      # nudge_y = 0.05,
      hjust=0.5,
      size=4
    ) +
    geom_hline(yintercept=0, color="grey80") +
    rcrea::theme_crea() +
    rcrea::scale_fill_crea_d() +
    labs(
      title="July 2024 year-over-year change in PM2.5 in Delhi, India",
      subtitle="Influence of weather and emissions",
      x=NULL, y="µg/m3",
      fill=NULL
    ) +
    scale_x_continuous(limits=c(-0,2)) +
    # hide x axis
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
  facet_wrap(~location_id)
    

  ggsave("doc/figures/deweathering_bar_yoy.png", width=10, height=6, scale=1.5)


  # Compare the two appraoches: trend and anomaly
  yoy_based_on_trend <- deweathered_trend_fire %>%
  filter(source=="cpcb") %>%
  select(location_id, result) %>%
  tidyr::unnest(result) %>%
  filter(floor_date(date, "month") %in% c(
    month_yoy %>% as.Date(),
    month_yoy %>% as.Date() - lubridate::years(1)
  )) %>%
  group_by(location_id, year=year(date), variable) %>%
  summarise(value=mean(value, na.rm=T)) %>%
  arrange(year) %>%
    group_by(across(-c(year, value))) %>%
    mutate(delta = value - lag(value)) %>%
    # Add observed value of first year
    ungroup() %>%
    group_by(location_id) %>%
  mutate(observed_prev = value[year == min(year) & variable == "observed"]) %>%
    ungroup() %>%
    filter(!is.na(delta)) %>%
    select(-c(value)) %>%
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "delta",
      names_prefix = "yoy_"
    ) %>%
    rename(
      yoy_total = yoy_observed,
      yoy_emission = yoy_trend,
    ) %>%
    mutate(
      yoy_weather = yoy_total - yoy_emission,
      yoy_total_rel = yoy_total / observed_prev,
      yoy_residual = yoy_anomaly / observed_prev,
      yoy_weather_rel = yoy_weather / observed_prev,
      yoy_emission_rel = yoy_emission / observed_prev
    ) %>%
    pivot_longer(
      cols = -c(year, observed_prev, location_id),
      names_to = "variable",
      values_to = "value",
    ) %>%
    filter(grepl("yoy.*_rel", variable))  %>%
    select(location_id, variable, value)


  
  
  yoy_based_on_anomaly <-  deweathered_yoy %>%
    filter(source=="cpcb") %>%
   select(location_id, result) %>%
    tidyr::unnest(result) %>%
    filter(grepl("yoy.*_rel", variable)) 

  
  bind_rows(
    yoy_based_on_trend %>% mutate(approach="Trend"),
    yoy_based_on_anomaly %>% mutate(approach="Anomaly")
  ) %>%
  mutate(variable_str=case_when(
      grepl("total", variable) ~ "Observed change yoy",
      grepl("weather", variable) ~ "Influence of weather",
      grepl("emission", variable) ~ "Influence of emissions"
    )) %>%
  ggplot(aes(y=value, x=approach)) +
    geom_col(data=function(x) filter(x, variable != "yoy_total_rel"),
              aes(fill=variable_str),
              width=0.5) +
    geom_point(data=function(x) filter(x, variable=="yoy_total_rel"), size=4) +
    geom_text(
      # data=function(x) filter(x, variable=="yoy_total_rel"),
      aes(label=glue("{variable_str}: {paste0(ifelse(value>0,'+',''), round(value, 2)*100,'%')}"),
          y=value,
          vjust=ifelse(value>0,-1,1)*1.5),
      # nudge_x = 0.0,
      # nudge_y = 0.05,
      hjust=0.5,
      size=4
    ) +
    geom_hline(yintercept=0, color="grey80") +
    rcrea::theme_crea() +
    rcrea::scale_fill_crea_d() +
    labs(
      title="July 2024 year-over-year change in PM2.5 in Delhi, India",
      subtitle="Influence of weather and emissions",
      x=NULL, y="µg/m3",
      fill=NULL
    ) +
    # scale_x_continuous(limits=c(-0,2)) +
    # hide x axis
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
  facet_wrap(~location_id)


}
