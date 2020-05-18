source('scripts/_load_dependencies.R')


samples <- 300
lag <- 0
exp_suffix <- NULL #'' #'_50'# NULL#'sampled1000'
normalise <- F
detect_breaks <- F
add_timestamp_var <- T
training_date_cut <- "2020-06-01"

# meas <- readRDS("~/development/crea/covid_impact/data/00_init/input/china_allstations_dailyAQ.RDS")
# stations <- read.csv("~/development/crea/covid_impact/data/00_init/input/china_stations.csv")
# 
# meas$date <- lubridate::date(meas$date)
# 
# data <- meas %>% dplyr::filter(date>="2016-01-01") %>% tidyr::gather("pollutant","value", -c(station_code, date)) %>%
#   group_by(station_code, pollutant) %>% tidyr::nest() %>% rename(meas=data)
# 
# data$unit <- "-"
# data$country <- "CN"
# data$iso2 <- "CN"
# 
# # Merge per city
# data_city <- tibble(data) %>%
#   merge(tibble(stations) %>%dplyr::select(station_code, CityEN, Lon, Lat), by=c("station_code")) %>%
#   group_by(CityEN, pollutant, country, iso2, unit) %>%
#   summarise(meas=list(bind_rows(meas) %>% group_by(date) %>% summarise(value=mean(value, na.rm=T))),
#             Lon=mean(Lon, na.rm=T),
#             Lat=mean(Lat, na.rm=T))
# 
# 
# data_city <- tibble(data_city) %>% rename(station_id=CityEN)
# data_city <- data_city[which(!is.na(data_city$Lat)),]
# data_city <- data_city[which(!is.na(data_city$Lon)),]
# 
# data_city_sf <- data_city %>% sf::st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
# ggplot(data_city_sf) + geom_sf()
# 
# meas_weather <- collect_weather(data_city_sf, years=seq(2016,2020), years_force_refresh = NULL,
#                                 pollutants=c("NO2","PM2.5","SO2","O3","PM10","CO"),
#                                 deg=NULL, n_per_station=3,
#                                 filename="china_meas_weather_3station_6polls.rds"
# 
# )
# meas_weather <- readRDS("data/01_weather/output/china_meas_weather_3station_6polls.rds")
# data <- prep_data(meas_weather=meas_weather, pollutants, deg,
#                    filename="china_meas_weather_3station_6polls.rds")
# 
# saveRDS(data, "data/02_prep_training/output/china_meas_weather_3station_6polls.rds")
data <- readRDS("data/02_prep_training/output/china_meas_weather_3station_6polls.rds")


trees <- c(20,50,100,300,600)
samples <- c(100)
lag <- c(0, 1, 2)
pollutants <- c("NO2","PM2.5")
engine <- c("deweather", "rmweather")
station_ids <- c("Tianjin","Wuhan")
weather_vars <- c(
  list(c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'ceil_hgt', 'precip', 'RH_max', 'pbl_max', 'sunshine'))
  # list(c('air_temp_min', 'air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'precip', 'RH_max', 'pbl_max'))
)


configs <-  tibble() %>%
  tidyr::expand(trees, lag, weather_vars, engine)

results <- configs %>% rowwise() %>%
  mutate(
    result=list(train_models(
      engine=engine,
      meas_weather=tibble(data) %>% dplyr::filter(station_id %in% station_ids,
                                                  pollutant %in% pollutants),
      weather_vars=weather_vars,
      trees=trees,
      samples=samples,
      lag=lag,
      normalise=normalise,
      detect_breaks=detect_breaks,
      add_timestamp_var=add_timestamp_var,
      exp_suffix=exp_suffix,
      training_date_cut=training_date_cut,
      save_result=F,
      return_result=T)
    ))


results <- results %>% tidyr::unnest(cols=c(result))

# output_folder <- file.path('data', '03_train_models', 'output')
saveRDS(results,file=file.path(output_folder, "china_parametric_wuhan_tianjin.RDS"))

# configs <- readRDS(file=file.path(output_folder, "china_parametric.RDS"))



# Analysis
# Mean Absolute Error
mae_test_plot_data <- results %>% rowwise() %>% mutate(mae_test=model$mae_test) %>%
  dplyr::select(station_id, pollutant, trees, lag, weather_vars, mae_test, engine)

ggplot(mae_test_plot_data %>% filter(station_id %in% c("Wuhan", "Beijing"))) +
  geom_point(aes(x=factor(trees),y=mae_test, color=engine)) +
  facet_grid(paste0(lag,"-day lag")~station_id+pollutant) +
  theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3)) +
  labs(title="Mean absolute error on test dataset")

rsq_test_plot_data <- results %>% rowwise() %>% mutate(rsquared_test=model$rsquared_test) %>%
  dplyr::select(station_id, pollutant, trees, lag, weather_vars, rsquared_test, engine)

ggplot(rsq_test_plot_data %>% filter(station_id %in% c("Wuhan", "Beijing"))) +
  geom_point(aes(x=factor(trees),y=rsquared_test, color=engine)) +
  facet_grid(paste0(lag,"-day lag")~station_id+pollutant) +
  theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3)) +
  labs(title="R^2 on test dataset")

# Prediction on test dataset
# pred_plot_data <- china_parametric %>% rowwise() %>% mutate(predicted=list(result %>% rowwise() %>% dplyr::select(station_id, pollutant, predicted))) %>% dplyr::select(trees, lag, weather_vars, predicted) %>% tidyr::unnest(cols=c(predicted)) %>% tidyr::unnest(cols=c(predicted))
# pred_plot_data <- pred_plot_data %>% filter(set="testing") %>% tidyr::gather("type","value",-c(trees, lag, weather_vars, station_id, pollutant))
# 
# ggplot(pred_plot_data %>% filter(station_id %in% c("Wuhan", "Beijing"))) +
#   geom_line(aes(x=value,y=partial_dependency, linetype=type, color=factor(paste(weather_vars)))) +
#   facet_grid(paste0(lag,"-day lag") ~ station_id+pollutant) +
#   theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3)) +
#   labs("Prediction on test dataset")


# Trend impact
trend_plot_data <- results %>% rowwise() %>% dplyr::select(station_id, pollutant, engine, trees, lag, weather_vars, trend) %>% tidyr::unnest(cols=c(trend))


ggplot(trend_plot_data %>% filter(station_id %in% c("Wuhan", "Beijing"))) + geom_line(aes(x=date,y=mean, color=engine, linetype=factor(trees))) + facet_grid(paste0(lag,"-day lag")~station_id+pollutant) + theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3))


