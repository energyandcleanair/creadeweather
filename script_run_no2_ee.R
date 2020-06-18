require(pbapply)
require(dplyr)
require(readr)
require(raster)
require(purrr)
require(sf)
require(raster)
require(pbapply)
require(bit64)
require(zoo)
require(ggnewscale)
require(lubridate)
require(gbm)
require(rnoaa)
require(e1071)
require(standardize)
require(Metrics)
require(stringr)
require(stats)
require(future)
require(rmarkdown)
require(countrycode)
require(magrittr)
require(ggthemes)
require(scales)
require(tibble)
require(worldmet)
require(sirad)
require(rmweather)
require(pbmcapply)
require(parallel)
require(cowplot)
require(ggpubr)

lapply(list.files('R','*.R',full.names = T), source)



pollutants <- c('NO2')
deg <- 0.5
trees <- 600
samples <- 300
lag <- 2
exp_suffix <- NULL #'' #'_50'# NULL#'sampled1000'
# sampled <- 1000
normalise <- F
add_timestamp_var <- F

csvs <- list.files(path="~/development/crea/ee_pollutants/results", pattern = "*deg0_5.csv", full.names = T)
data_no2 <- do.call("bind_rows",lapply(csvs, read.csv, stringsAsFactors=FALSE))
data_no2$date <- strptime(data_no2$date,format="%Y-%m-%d %H:%M:%S", tz="UTC")
data_no2$date <- lubridate::date(data_no2$date)
data_no2$pollutant <- "NO2"
data_no2$unit <- "mol/m2"
data_no2$country <- data_no2$region_id
data_no2$iso2 <- data_no2$region_id

# Temporary (KH file is corrupted)
data_no2 <- data_no2[which(!is.na(data_no2$latitude)),]
data_no2 <- data_no2[which(!is.na(data_no2$longitude)),]


# For some reason, grouping by station_id, lat, lon gives two rows per station although lat long seem equal.
# Not a good idea to group by floats anyway. So doing it in two steps instead.
data_no2_nested <- data_no2 %>%
  dplyr::select(-c(longitude,latitude)) %>%
  group_by(station_id, country, pollutant, unit) %>%
  tidyr::nest() %>%
  rename(meas=data) %>%
  dplyr::left_join(
    data_no2 %>%
      dplyr::select(station_id, country, longitude, latitude) %>%
      distinct(station_id, country, .keep_all = T)
  )

data_no2_nested_sf <- data_no2_nested %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
ggplot(data_no2_nested_sf) + geom_sf()

meas_weather <- collect_weather(data_no2_nested_sf, years=seq(2018,2020),
                                pollutants=c("NO2"), deg=0.5, n_per_station=2,
                                filename='SEA_TROPOMI_deg0_5.rds'
                                )

data <- prep_data(meas_weather=meas_weather, pollutants, deg)
data$unit <- "mol/m2" # Was missing early one. Can remove next time.

ggplot(st_as_sf(data %>% rowwise() %>% mutate(nvalid=nrow(meas_weather %>% filter(!is.na(air_temp)))) %>% filter(nvalid>0))) +
  geom_sf(aes(color=nvalid))

result_folder <- train_models(
  engine="rmweather",
  meas_weather=tibble(data),
  pollutants=pollutants,
  deg=deg,
  trees=trees,
  samples=samples,
  lag=lag,
  normalise=normalise,
  detect_breaks=detect_breaks,
  add_timestamp_var=add_timestamp_var,
  exp_suffix=exp_suffix)

postcompute_results_rmweather(result_folder)
