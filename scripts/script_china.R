source('scripts/_load_dependencies.R')

polls <- c(rcrea::NO2, rcrea::PM25) #c("NO2", "PM2.5")
# station_ids <- c("Tianjin","Wuhan","Beijing","Urumqi","Shanghai","Shijiazhuang")
station_ids <- c("Delhi","Mumbai","Hyderabad","Jaipur","Kolkata")

meas <- rcrea::measurements(poll=polls,
                            location_id=station_ids,
                            aggregate_level="city",
                            date_from="2016-01-01",
                            source='cpcb',
                            deweathered=F,
                            with_geometry=T) %>%
  group_by(region_id, poll, unit, source, timezone, process_id, geometry) %>%
  tidyr::nest() %>%
  mutate(country="IN") %>% rename(station_id=region_id, meas=data) %>% ungroup()

meas_sf <- meas %>% ungroup() %>%
  mutate(geometry=sf::st_centroid(sf::st_as_sfc(geometry))) %>%
  st_as_sf(sf_column_name="geometry", crs = 4326)

meas_weather <- collect_weather(meas_sf,
                                years=seq(2017,2020),
                                years_force_refresh = 2020,
                                add_pbl=T,
                                n_per_station=3
)

data <- prep_data(meas_weather=meas_weather)

exp_suffix <- "china6_gbm"
normalise <- F
detect_breaks <- F
training_start <- "2017-01-01"
training_end <- "2020-01-01"
trees <- 100#c(50,300)
samples <- 100
lag <- c(0,1) #c(0, 1, 2)
engine <- "gbm" #c("deweather")#,"rmweather")
weather_vars <- c(
  # list(c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH_max', 'pbl_max', 'sunshine')),
  list(c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH_max')),
  list(c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH_max', 'pbl_max'))
)
time_vars <- c(
  list(c(NULL)),
  list(c('yday')),
  list(c('trend','yday'))
  # list(c('trend','yday','month')),
  # list(c('trend','yday','week')),
  # list(c('trend','month','wday')) 
)

configs <-  tibble() %>%
  tidyr::expand(trees, lag, weather_vars, time_vars, engine) %>%
  mutate(process_deweather=
           gsub("'","\"",paste0("{'engine':'",engine,"',",
                                "'trees':'",trees,"',",
                                "'lag':'",lag,"',",
                                "'training_start':'",training_start,"',",
                                "'training_end':'",training_end,"',",
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
      training_date_cut=training_start,
      save_result=F,
      return_result=T)
    ))


results <- results_nested %>% tidyr::unnest(cols=c(result))

# print("Saving results")
# output_folder <- file.path('data', '03_train_models', 'output')
# saveRDS(results,file=file.path(output_folder, "china_parametric_normalised_6city.RDS"))
# 
# results <- readRDS(file=file.path(output_folder, "china_parametric_normalised_6city.RDS"))
# 
# 
# 
# Analysis
# Mean Absolute Error
# mae_test_plot_data <- results %>% rowwise() %>% mutate(mae_test=model$mae_test) %>%
#   dplyr::select(station_id, poll, trees, lag, weather_vars, mae_test, engine)
# 
# ggplot(mae_test_plot_data) +
#   geom_point(aes(x=factor(trees),y=mae_test, color=engine)) +
#   facet_grid(paste0(lag,"-day lag")~station_id+poll) +
#   theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3)) +
#   labs(title="Mean absolute error on test dataset")
# 
# rsq_test_plot_data <- results %>% rowwise() %>% mutate(rsquared_test=model$rsquared_test) %>%
#   dplyr::select(station_id, poll, trees, lag, weather_vars, rsquared_test, engine)
# 
# ggplot(rsq_test_plot_data) +
#   geom_point(aes(x=factor(trees),y=rsquared_test, color=engine)) +
#   facet_grid(paste0(lag,"-day lag")~station_id+poll) +
#   theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3)) +
#   labs(title="R^2 on test dataset")

# Plot residuals
pred_plot_data <- results %>% rowwise() %>%
  dplyr::select(station_id, trees, lag, poll, unit, weather_vars, predicted, time_vars) %>%
  dplyr::mutate(config=paste(trees,lag,paste(weather_vars,collapse = "_"),paste(time_vars,collapse = "_"))) %>%
  tidyr::unnest(cols=c(predicted)) %>%
  mutate(residuals=predicted-value) %>%
  utils.rolling_average(average_by="day",average_width =7,
                        group_cols=c("station_id", "poll", "unit", "config","set"),
                        avg_cols=c("predicted","value","residuals")
                        )

ggplot(pred_plot_data) +
  geom_line(aes(x=date,y=residuals, linetype=set, color=config)) +
  geom_vline(xintercept = as.POSIXct(lubridate::date('2020-03-25')), linetype='dotted') +
  facet_grid(station_id ~ poll) +
  theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 5)) +
  labs("Prediction on test dataset")




# 
# # Trend impact
# trend_plot_data <- results %>% filter(station_id %in% c("Wuhan", "Beijing", "Tianjin", "Shanghai", "Urumqi")) %>%
#   rowwise() %>%
#   mutate(weather_vars=paste(weather_vars,collapse=",")) %>%
#   dplyr::select(station_id, poll, engine, trees, lag, weather_vars, trend) %>%
#   tidyr::unnest(cols=c(trend)) %>%
#   rowwise() %>%
#   mutate(date=lubridate::date(date)) %>%
#   group_by(station_id, poll, engine, trees, lag, weather_vars, date) %>%
#   summarise_at(c("mean","lower","upper"),mean, na.rm=T)
# 
# 
# ggplot(trend_plot_data) +
#   geom_line(aes(x=date,y=mean, color=engine, linetype=factor(trees))) +
#   facet_grid(paste0(lag,"-day lag") + weather_vars~station_id+poll) +
#   theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3))
# 
# 
# # Weather-normalised
# normalised_plot_data <- results %>%
#   rowwise() %>%
#   mutate(weather_vars=paste(weather_vars,collapse=","),
#          time_vars=paste(time_vars,collapse=",")) %>%
#   dplyr::select(station_id, poll, engine, trees, lag, weather_vars, time_vars, normalised) %>%
#   tidyr::unnest(cols=c(normalised)) %>%
#   mutate(date=lubridate::date(date)) %>%
#   # utils.rolling_average(average_by="day",average_width=0,
#   #                       group_cols=c("station_id","poll","engine","trees","lag","weather_vars","time_vars"),
#   #                       avg_cols=c("value_predict")) %>%
#   mutate(type="2.normalized") %>%
#   rename(value=value_predict)
# 
# observed_plot_data <- data  %>%
#   dplyr::filter(station_id %in% station_ids, poll %in% polls)%>% 
#   dplyr::select(-c(country)) %>%
#   tidyr::unnest(meas_weather) %>%
#   # utils.rolling_average(average_by="day",average_width=0,
#   #                       group_cols=c("station_id","poll","unit"),
#   #                       avg_cols=c("value")) %>%
#   merge(normalised_plot_data %>% distinct(engine, trees, lag, weather_vars, time_vars)) %>%
#   mutate(type="1.observed")
# 
# ggplot(bind_rows(observed_plot_data,normalised_plot_data) %>% filter(poll %in% polls)) +
#   geom_line(aes(x=date,y=value, color=type, alpha=type)) +
#   facet_grid(paste0(lag,"-day lag") + station_id + engine + trees ~ poll+weather_vars+time_vars) +
#   theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3)) +
#   scale_alpha_manual(values=c(0.5,1))

# 
# 
# # Upload results
processes <- results %>% distinct(process_id, process_deweather)

upload_process_meas <- function(meas_process_id, process_deweather, poll, unit, station_id, normalised, source){

  # Create or retrieve process
  meas_process <- rcrea::processes() %>% filter(id==meas_process_id) %>% collect()
  deweather_process_id = rcrea::retrieve_or_create_process(meas_process$filter[[1]],
                                                           meas_process$agg_spatial[[1]],
                                                           meas_process$agg_temp[[1]],
                                                           deweather=process_deweather,
                                                           prefix_if_not_exists='deweather'
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


