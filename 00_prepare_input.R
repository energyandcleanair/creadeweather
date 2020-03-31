require(dplyr)
require(purrr)
require(sf)
require(raster)
require(tidyr)

meas <- readRDS('data/00_init/daily data all stations.RDS')
meas <- tibble(meas)

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

stations_meta <- read.csv('data/00_init/input/PanEuropean_metadata_city.csv', header=T)

#------------------------------------------------
# Extract geometries for GEE to get weather
#------------------------------------------------
station_codes <- unique(meas_meta$station) 
length(station_codes) #2860
stations <- tibble(AirQualityStation=station_codes) %>%
  left_join(stations_meta %>%
              dplyr::select(AirQualityStation, Latitude, Longitude) %>%
              distinct(AirQualityStation, .keep_all=T)
            )
stations_sf <- sf::st_as_sf(stations, coords=c("Longitude","Latitude"),crs=4326)

# Add GADM1 information
iso2 <- unique(meas_meta$country) # Used for filtering GADM1 in QGIS
iso3 <- countrycode::countrycode(iso2, "iso2c", "iso3c")
gadm1s <- lapply(iso3, function(x) tryCatch({raster::getData('GADM', country=x, level=1)},error=function(cond){NULL}))
gadm1s <- gadm1s[!sapply(gadm1s,is.null)]
gadm1 <- do.call(raster::bind, gadm1s) 
gadm1_sf <- sf::st_as_sf(gadm1) %>% dplyr::select(gadm1_id=GID_1, gadm1_name=NAME_1)
stations_sf <- st_join(stations_sf, gadm1_sf)
gadm1_filtered_sf <- gadm1_sf %>% filter(gadm1_id %in% unique(stations_sf$gadm1_id))
# Gee doesn't like complex shapes (takes a lot of time). We simplify GADM1s as their boundinx boxes
gadm1_filtered_bounds_sf <- gadm1_filtered_sf %>% rowwise() %>% mutate(geometry=st_bounds(geometry))

# Export to geojson (or shapefile)
file.remove(list.files('data/00_init/output/','^stations.(geojson|shp|prj|dbf|shx)', full.names = T))
stations_sf %>% sf::st_write('data/00_init/output/stations.shp')
stations_sf %>% sf::st_write('data/00_init/output/stations.geojson')

file.remove(list.files('data/00_init/output/','^gadm1_filtered.(geojson|shp|prj|dbf|shx)', full.names = T))
gadm1_filtered_sf %>% sf::st_write('data/00_init/output/gadm1_filtered.shp')
gadm1_filtered_sf %>% sf::st_write('data/00_init/output/gadm1_filtered.geojson')

file.remove(list.files('data/00_init/output/','^gadm1_filtered_bounds.(geojson|shp|prj|dbf|shx)', full.names = T))
gadm1_filtered_bounds_sf %>% sf::st_write('data/00_init/output/gadm1_filtered_bounds.shp')
gadm1_filtered_bounds_sf %>% sf::st_write('data/00_init/output/gadm1_filtered_bounds.geojson')

#------------------------------
# Save stations with/AT GADM1s
#------------------------------
# Before: meas grouped by station and year
# After: meas grouped by station only
meas_station <- meas %>% rowwise() %>%
  mutate(station=unique_w_na(meas$AirQualityStation)) %>%
  filter(!is.na(station)) %>%
  group_by(station) %>%  summarise(meas=list(bind_rows(meas)))

# w_gadm: 'with gadm' measurements are kept at the station level (gadm reference is simply added)
# at_gadm: 'at gadm': measurements are AVERAGED within gadms
meas_station_w_gadm <- meas_station %>% left_join(stations_sf %>% dplyr::rename(station=AirQualityStation)) %>%
                             dplyr::select(station, gadm1_id, gadm1_name, meas) %>%
                             filter(!is.na(gadm1_id)) # Remove Gibraltar
saveRDS(meas_station,'data/00_init/output/meas_w_gadm.RDS')

# meas_at_gdam <- meas_station_w_gadm %>% tidyr::unnest(cols=meas)
meas_at_gdam <- meas_station_w_gadm %>%
  group_by(gadm1_id, gadm1_name) %>%
  summarise(meas=list(
              bind_rows(meas) %>%
              group_by(date, AirPollutant) %>%
              summarise(Concentration=mean(Concentration, na.rm=T))
            ))
saveRDS(meas_at_gdam,'data/00_init/output/meas_at_gadm.RDS')
