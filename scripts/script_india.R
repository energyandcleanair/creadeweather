source('scripts/_load_dependencies.R')
 
city <- "Delhi" #NULL if all of them



meas <- rcrea::measurements(city=city, source='cpcb', average_by = "day", aggregate_level = "city", date_from="2015-01-01",
                            with_metadata = T, with_geometry=T, deweathered = F) %>%
  group_by(region_id, poll, unit, source, timezone, region_name, process_id, geometry) %>%
  tidyr::nest() %>%
  mutate(country="IN") %>% rename(station_id=region_id, meas=data) %>% ungroup()

meas$geometry = st_centroid(sf::st_as_sfc(meas$geometry))
meas_sf <- meas %>% sf::st_as_sf(crs = 4326)

meas_weather <- collect_weather(meas_sf,
                                years=seq(2015,2020),
                                years_force_refresh = 2020,
                                n_per_station=5,
                                add_pbl=F,
                                filename="india_meas_weather_5station.rds"
)

data <- prep_data(meas_weather=meas_weather,
                  filename="india_meas_weather_3station.rds")

# data <- readRDS("data/02_prep_training/output/india_meas_weather_3station.rds")

# Removing last dates without weather (that may mislead reader e.g. if rebound not shown)
data <- data  %>% mutate(meas_weather=list(meas_weather %>% filter(!is.na(air_temp_min))))


exp_suffix <- "india_normalised"
normalise <- T
detect_breaks <- F
add_timestamp_var <- T
add_yday <- T
training_date_cut <- "2020-06-01"
trees <- 300 
samples <- 100
lag <- 2
# polls <- c("no2","pm25", "co", "")
engine <- c("deweather")
station_ids <- c("Delhi","Jaipur","Varanasi","Kolkata","Mumbai","Hyderabad")
weather_vars <- c(
  # list(c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH_max', 'pbl_max', 'sunshine')),
  list(c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH_max'))
)
time_vars <- c(
  list(c('trend','yday'))
  # list(c('trend','yday','month')),
  # list(c('trend','yday','week')),
  # list(c('trend','month','wday'))
)

configs <-  tibble() %>%
  tidyr::expand(trees, lag, weather_vars, time_vars, engine)

# Adding process json definition for storage in creadb
configs <- configs %>% rowwise() %>% mutate(process_deweather=
                     gsub("'","\"",paste0("{'engine':'",engine,"',",
                                          "'trees':'",trees,"',",
                                          "'lag':'",lag,"',",
                                          "'time_vars':['",paste0(time_vars,collapse="','"),"'],",
                                          "'weather_vars':['",paste0(weather_vars,collapse="','"),"']",
                                          "}")
                          )
                     
                     )
results_nested <- configs %>% rowwise() %>%
  mutate(
    result=list(train_models(
      engine=engine,
      meas_weather=tibble(data),
      weather_vars=weather_vars,
      time_vars=time_vars,
      trees=trees,
      samples=samples,
      lag=lag,
      normalise=normalise,
      detect_breaks=detect_breaks,
      exp_suffix=exp_suffix,
      training_date_cut=training_date_cut,
      save_result=F,
      return_result=T)
    ))

output_folder <- file.path('data', '03_train_models', 'output')
saveRDS(results_nested,file=file.path(output_folder, "india_normalised"))
# results_nested <- readRDS(file=file.path(output_folder, "india_normalised"))
results <- results_nested %>% tidyr::unnest(cols=c(result))

# # 
# # 
# # 
# # Analysis
# # Mean Absolute Error
# mae_test_plot_data <- results %>% rowwise() %>% mutate(mae_test=model$mae_test) %>%
#   dplyr::select(station_id, pollutant, trees, lag, weather_vars, mae_test, engine)
# 
# ggplot(mae_test_plot_data %>% filter(station_id %in% c("Wuhan", "Beijing"))) +
#   geom_point(aes(x=factor(trees),y=mae_test, color=engine)) +
#   facet_grid(paste0(lag,"-day lag")~station_id+pollutant) +
#   theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3)) +
#   labs(title="Mean absolute error on test dataset")
# 
# rsq_test_plot_data <- results %>% rowwise() %>% mutate(rsquared_test=model$rsquared_test) %>%
#   dplyr::select(station_id, pollutant, trees, lag, weather_vars, rsquared_test, engine)
# 
# ggplot(rsq_test_plot_data %>% filter(station_id %in% c("Wuhan", "Beijing"))) +
#   geom_point(aes(x=factor(trees),y=rsquared_test, color=engine)) +
#   facet_grid(paste0(lag,"-day lag")~station_id+pollutant) +
#   theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3)) +
#   labs(title="R^2 on test dataset")
# 
# # Prediction on test dataset
# # pred_plot_data <- china_parametric %>% rowwise() %>% mutate(predicted=list(result %>% rowwise() %>% dplyr::select(station_id, pollutant, predicted))) %>% dplyr::select(trees, lag, weather_vars, predicted) %>% tidyr::unnest(cols=c(predicted)) %>% tidyr::unnest(cols=c(predicted))
# # pred_plot_data <- pred_plot_data %>% filter(set="testing") %>% tidyr::gather("type","value",-c(trees, lag, weather_vars, station_id, pollutant))
# #
# # ggplot(pred_plot_data %>% filter(station_id %in% c("Wuhan", "Beijing"))) +
# #   geom_line(aes(x=value,y=partial_dependency, linetype=type, color=factor(paste(weather_vars)))) +
# #   facet_grid(paste0(lag,"-day lag") ~ station_id+pollutant) +
# #   theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3)) +
# #   labs("Prediction on test dataset")
# 
# 
# # Trend impact
# trend_plot_data <- results %>% rowwise() %>%
#   dplyr::select(station_id, pollutant, engine, trees, lag, weather_vars, trend) %>%
#   tidyr::unnest(cols=c(trend)) %>%
#   mutate(date=lubridate::date(date), weather_vars=paste(weather_vars, collapse=",")) %>%
#   group_by(station_id, pollutant, engine, trees, lag, weather_vars, date) %>%
#   summarise_at(c("mean","lower","upper"), mean, na.rm=T)
# 
# ggplot(trend_plot_data) +
#   geom_line(aes(x=date,y=mean, color=engine, linetype=factor(trees))) +
#   facet_grid(paste0(lag,"-day lag") + weather_vars ~ station_id+pollutant) +
#   theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3))
# # Weather-normalised
# normalised_plot_data <- results %>%
#   rowwise() %>%
#   mutate(weather_vars=paste(weather_vars,collapse=","),
#          time_vars=paste(time_vars,collapse=",")) %>%
#   dplyr::select(station_id, poll, engine, trees, lag, weather_vars, time_vars, normalised) %>%
#   tidyr::unnest(cols=c(normalised)) %>%
#   mutate(date=lubridate::date(date)) %>%
#   utils.rolling_average(average_by="day",average_width=14,
#                         group_cols=c("station_id","poll","engine","trees","lag","weather_vars","time_vars"),
#                         avg_cols=c("value_predict")) %>%
#   mutate(type="normalized") %>%
#   rename(value=value_predict)
# 
# observed_plot_data <- data %>%
#   filter(tolower(station_id) %in% tolower(station_ids)) %>%
#   dplyr::select(-c(country)) %>%
#   filter(poll %in% polls) %>%
#   tidyr::unnest(meas_weather) %>%
#   utils.rolling_average(average_by="day",average_width=14,
#                         group_cols=c("station_id","poll","unit"),
#                         avg_cols=c("value")) %>%
#   merge(normalised_plot_data %>% distinct(engine, trees, lag, weather_vars, time_vars)) %>%
#   mutate(type="observed")
# 


# 
# ggplot(bind_rows(normalised_plot_data, observed_plot_data) %>% filter(type=="normalized") ) +
#   geom_line(aes(x=date,y=value, linetype=engine, color=type)) +
#   # geom_line(data = original_plot_data, aes(x=date,y=value)) +
#   #geom_line(aes(x=date,y=pred, color=engine, linetype=factor(trees))) +
#   facet_grid(paste0(lag,"-day lag") + station_id + engine~poll+weather_vars+time_vars) +
#   theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3))

# Upload results
processes <- results %>% distinct(process_id, process_deweather)

upload_process_meas <- function(meas_process_id, process_deweather, poll, unit, station_id, normalised, source){
  
  # Create or retrieve process
  meas_process <- rcrea::processes() %>% filter(id==meas_process_id) %>% collect()
  deweather_process_id = rcrea::retrieve_or_create_process(meas_process$filter[[1]],
                                    meas_process$agg_spatial[[1]],
                                    meas_process$agg_temp[[1]],
                                    deweather=process_deweather
                                    )
  
  normalised_meas <- normalised %>%
    rename(value=value_predict) %>%
    mutate(region_id=station_id,
           poll=poll,
           unit=unit,
           process_id=deweather_process_id,
           source=source)
  
  rcrea::upsert_meas(normalised_meas)
  return(deweather_process_id)
}

results_upload <- results %>% rowwise() %>%
  mutate(deweather_process_id=upload_process_meas(process_id,process_deweather,poll,unit,station_id,normalised,source))



