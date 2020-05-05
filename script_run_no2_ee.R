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
deg <- 0.2
trees <- 600
samples <- 300
lag <- 2
exp_suffix <- NULL #'' #'_50'# NULL#'sampled1000'
# sampled <- 1000
normalise <- F
add_timestamp_var <- F


data_no2 <- tibble(read.csv("/Users/ht/development/crea/scripts/ee_pollutants/results/KH_20180701_20200421_deg0_5.csv",
                            stringsAsFactors=FALSE))
data_no2$date <- strptime(data_no2$date,format="%Y-%m-%d %H:%M:%S", tz="UTC")
data_no2$date <- lubridate::date(data_no2$date)
data_no2$pollutant <- "NO2"
data_no2$iso2 <- "KH"

# For some reason, grouping by station_id, lat, lon gives two rows per station although lat long seem equal.
# Not a good idea to group by floats anyway. So doing it in two steps instead.
data_no2_nested <- data_no2 %>%
  dplyr::select(-c(longitude,latitude)) %>%
  group_by(station_id) %>%
  tidyr::nest() %>%
  rename(meas=data) %>%
  dplyr::left_join(
    data_no2 %>%
      dplyr::select(station_id, longitude, latitude) %>%
      distinct(station_id, .keep_all = T)
  )

data_no2_nested_sf <- data_no2_nested %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
meas_weather <- collect_weather(data_no2_nested_sf, pollutants=c("NO2"), deg=0.5, n_per_station=2)

