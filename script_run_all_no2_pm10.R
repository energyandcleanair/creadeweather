# Ensure we have latest code, even if we didn't rebuild package
lapply(list.files('R','*.R',full.names = T), source)


pollutants <- c('NO2','PM10')
deg <- 0.2
trees <- 600
samples <- 300
lag <- 2
exp_suffix <- NULL #'' #'_50'# NULL#'sampled1000'
# sampled <- 1000
normalise <- F
add_timestamp_var <- F


data_no2 <- readRDS("~/development/crea/crea_weather_aq_gadm1/data/02_prep_training/output/meas_w_weather_no2_02_.RDS") %>%
  filter(nrow(meas_weather) >= 800) %>% filter(max(meas_weather$date)>'2020-01-01')

data_pm10 <- readRDS("~/development/crea/crea_weather_aq_gadm1/data/02_prep_training/output/meas_w_weather_pm10_02_.RDS") %>%
  filter(nrow(meas_weather) >= 800) %>% filter(max(meas_weather$date)>'2020-01-01')

#shared_stations <- intersect(data_no2$station_id,data_pm10$station_id)
data_filtered <- bind_rows(data_no2, data_pm10)#%>% filter(station_id %in% shared_stations)
# 
# data_filtered_sf <- data_filtered %>% select(station_id, geometry) %>%
#   distinct(station_id, .keep_all = T) %>%
#   st_as_sf(crs=4326)
# 
# # Sample points to reduce computattion time
# geom_sample <- sf::st_sample(data_filtered_sf, size=sampled, type='regular')
# st_crs(geom_sample) = 4326
# data_filtered_sampled_sf <- st_as_sf(geom_sample) %>% st_join(data_filtered_sf, join=st_intersects)
# ggplot(data_filtered_sampled_sf) + geom_sf()
# ggplot(data_filtered_sf) + geom_sf()
# 
# data_filtered <- data_filtered %>% filter(station_id %in% data_filtered_sampled_sf$station_id)
# Only select sample
# 
data <- crealockdown::prepare_input(pollutants=pollutants, deg=deg, years_force_refresh = NULL)
# data <- get_weather(meas=data,pollutants,deg, force_refresh_2020=F)
# 

# data <- prep_training(meas_weather=data, pollutants, deg)
result_folder <- train_models_rmweather(meas_weather=tibble(data_filtered),
                                        pollutants=pollutants,
                                        deg=deg,
                                        trees=trees,
                                        samples=samples,
                                        lag=lag,
                                        normalise=normalise,
                                        add_timestamp_var=add_timestamp_var,
                                        exp_suffix=exp_suffix)
postcompute_results(result_folder)
