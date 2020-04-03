require(purrr)
require(sf)
require(raster)
require(tidyverse)
require(magrittr)
require(ggplot2)
require(countrycode)
require(lwgeom)

source('00_prepare_input_gadm1.R')
source('00_prepare_input_eea.R')
source('00_prepare_input_lombardia.R')

set.seed(2020)


cache_folder <- file.path('data', '00_init', 'cache')
if(!dir.exists(cache_folder)) dir.create(cache_folder, recursive = T)

output_folder <- file.path('data', '00_init', 'output')
if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)

input_folder <- file.path('data', '00_init', 'input')
if(!dir.exists(input_folder)) dir.create(input_folder, recursive = T)


# Get stations and regions
stations_eea_sf <- eea.get_stations_sf()
stations_lombardia_sf <- lombardia.get_stations_sf()

gadm1_sf <- gadm1.get_sf(eea_stations_sf$iso2)
gadm1_sf$area <- st_area(gadm1_sf)

# Select stations to keep
# (we can't keep all of them)
min_station_per_gadm1 <- 1
avg_station_per_gadm1 <- 5
max_station_per_gadm1 <- 10
median_area <- median(gadm1_sf$area)
gadm1_sf$n_station <- pmin(max_station_per_gadm1,
                           pmax(min_station_per_gadm1,
                                as.integer(round(gadm1_sf$area/median_area*avg_station_per_gadm1))))

# Attach GADM1 to eea_stations
eea_stations_sf <- st_join(eea_stations_sf, gadm1_sf)

eea_stations_filtered <-
  data.frame(eea_stations_sf) %>%
  filter(date_to=='present') %>%
  group_by(gadm1_id) %>%
  filter(!is.na(n_station)) %>%
  mutate(n_station=min(n(), n_station)) %>%
  dplyr::sample_frac(1) %>%
  slice(1:unique(n_station))

nrow(eea_stations_filtered) # 2689 (min:1, median:5, max: 10) | 4488 (min:2, median:10, max: 20)
nrow(eea_stations_sf) # 22339
length(unique(eea_stations_filtered$gadm1_id)) #493
length(unique(eea_stations_sf$gadm1_id)) #498

# We download first
pollutant_names <- c("NO2", "CO", "PM10")
eea.download_stations_meas(unique(eea_stations_filtered$station_id),
                      pollutant_names=pollutant_names,
                      years_force_refresh=c(2020)
                      )

# then read
eea_meas <- eea.read_stations_meas(station_ids=unique(eea_stations_filtered$station_id),
                                                      pollutant_names=names(pollutants))
  
# then nest
eea_meas_nested <- eea_meas %>%
  group_by(station_id, pollutant) %>%
  tidyr::nest() %>%
  rename(meas=data)

# join GADM1
eea_stations_sf %>%
  select(station_id, gadm0_id, gadm1_id, geometry) %>%
  right_join(eea_meas_nested)

eea_meas_nested <- eea_meas_nested %>%
  left_join(
    eea_stations_sf %>% select(station_id, gadm0_id, gadm1_id, geometry)
  ) 

# then join gadm
eea_stations_sf
eea_stations_filtered <-





saveRDS(eea_meas, file.path(output_folder,'eea_meas_daily.RDS'))




# Add Lombardia
lombardia_meas <- lombardia.get_meas(pollutant_names)




# Select which stations to query (can't query all of them)

# meas <- readreadRDS(file.path('data','00_init','input','daily data all stations.RDS'))
# meas <- readreadRDS(file.path('data','00_init','input','daily data all stations.RDS'))

# Get unique countries, date interval etc.
unique_w_na <- function(values){
  res <- unique(values)
  return(ifelse(length(res)==1, res, NA))
}

meas_meta <- meas %>% rowwise() %>%
  mutate(
    country=unique_w_na(meas$Countrycode),
    station=unique_w_na(meas$AirQualityStation),
    date_min=min(meas$date),
    date_max=max(meas$date)) %>%
  group_by(country, station) %>%
  summarise(date_min=min(date_min), date_max=max(date_max)) %>%
  filter(!is.na(station))

stations_meta <- read.csv(file.path('data','00_init','input','PanEuropean_metadata_city.csv'), header=T)

#------------------------------------------------
# Extract geometries for GEE to get weather
#------------------------------------------------
station_codes <- unique(meas_meta$station) 
length(station_codes) #2860
stations <- tibble(station=station_codes) %>%
  left_join(stations_meta %>%
              dplyr::select(AirQualityStation, Latitude, Longitude) %>%
              distinct(AirQualityStation, .keep_all=T) %>%
              rename(station=AirQualityStation)
            )
stations_sf <- sf::st_as_sf(stations, coords=c("Longitude","Latitude"),crs=4326)

# Add GADM1 information
iso2 <- unique(meas_meta$country) # Used for filtering GADM1 in QGIS
iso3 <- countrycode::countrycode(iso2, "iso2c", "iso3c")
iso3 <- setdiff(iso3, "GIB")
# gadm1s <- GADMTools::gadm_sf.loadCountries(iso3, level=1, simplify = 0.05)
gadm1s <- lapply(iso3, function(x) tryCatch({raster::getData('GADM', country=x, level=1)},error=function(cond){NULL}))
gadm1s <- gadm1s[!sapply(gadm1s,is.null)]
gadm1 <- do.call(raster::bind, gadm1s) 
gadm1_sf <- st_as_sf(gadm1) %>% dplyr::select(gadm0_id=GID_0, gadm1_id=GID_1, gadm1_name=NAME_1)
gadm1_simplified_sf <- st_simplify(gadm1_sf,preserveTopology = T,dTolerance = 0.1)

stations_sf <- st_join(stations_sf, gadm1_sf)

# Some stations don't have a code: these are outlying islands (e.g. La Reunion)
n_missing <- nrow(stations_sf %>% filter(is.na(gadm1_id)))

gadm1_filtered_sf <- gadm1_simplified_sf %>% filter(gadm1_id %in% unique(stations_sf$gadm1_id))
# Gee doesn't like complex shapes (takes a lot of time). We simplify GADM1s as their boundinx boxes
gadm1_filtered_bounds_sf <- gadm1_filtered_sf %>% rowwise() %>% mutate(geometry=st_as_sfc(st_bbox(geometry)))

# Export to geojson (or shapefile)
file.remove(list.files(file.path('data','00_init','output'),'^stations.(geojson|shp|prj|dbf|shx)', full.names = T))
stations_sf %>% sf::st_write(file.path('data','00_init','output','stations.shp'))
stations_sf %>% sf::st_write(file.path('data','00_init','output','stations.geojson'))

file.remove(list.files(file.path('data','00_init','output'),'^gadm1_filtered.geojson', full.names = T))
gadm1_filtered_sf %>% sf::st_write(file.path('data','00_init','output','gadm1_filtered.geojson'))

file.remove(list.files(file.path('data','00_init','output'),'^gadm1.geojson', full.names = T))
gadm1_simplified_sf %>% sf::st_write(file.path('data','00_init','output','gadm1.geojson'))

file.remove(list.files(file.path('data','00_init','output'),'^gadm1_filtered_bounds.(geojson|shp|prj|dbf|shx)', full.names = T))
gadm1_filtered_bounds_sf %>% sf::st_write(file.path('data','00_init','output','gadm1_filtered_bounds.shp'))
gadm1_filtered_bounds_sf %>% sf::st_write(file.path('data','00_init','output','gadm1_filtered_bounds.geojson'))

#------------------------------
# Save stations with/AT GADM1s
#------------------------------
# Before: meas grouped by station and year
# After: meas grouped by station and pollutant
meas_station <- meas %>% rowwise() %>%
  mutate(station=unique_w_na(meas$AirQualityStation),
         pollutant=unique_w_na(meas$AirPollutant)) %>%
  group_by(station, pollutant) %>%
  summarise(meas=list(bind_rows(meas)))

# Check we're not missing any (which could happen if original measurements
# were not properly grouped by station and pollutant)  
n_missed <- sum((meas_station %>%
  filter(is.na(station) || is.na(pollutant)) %>%
  rowwise() %>%
  mutate(total=nrow(meas)))$total)
if(n_missed>0) stop("Measurement data was not properly grouped")


# w_gadm: 'with gadm' measurements are kept at the station level (gadm reference is simply added)
# at_gadm: 'at gadm': measurements are AVERAGED within gadms
meas_station_w_gadm <- meas_station %>%
  left_join(stations_sf) %>%
  filter(!is.na(gadm1_id)) %>%
  dplyr::select(station, gadm0_id, gadm1_id, gadm1_name, pollutant, meas)

saveRDS(meas_station_w_gadm,file.path('data','00_init','output','meas_w_gadm.RDS'))
write.csv(meas_station %>% tidyr::unnest(cols=c(meas)),file.path('data','00_init','output','meas_w_gadm.csv'))

# meas_at_gdam <- meas_station_w_gadm %>% tidyr::unnest(cols=meas)
meas_at_gadm <- meas_station_w_gadm %>%
  group_by(gadm0_id, gadm1_id, gadm1_name, pollutant) %>%
  summarise(meas=list(
              bind_rows(meas) %>%
              group_by(date) %>%
              summarise(value=mean(Concentration, na.rm=T))
            ))
saveRDS(meas_at_gadm,file.path('data','00_init','output','meas_at_gadm.RDS'))

# Show how many measurements we have per region / pollutant
plot.map_count(meas_at_gadm,
               folder=file.path('data', '00_init', 'output'),
               title='Number of measurements',
               meas_col='meas')
