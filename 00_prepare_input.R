require(purrr)
require(sf)
require(raster)
require(tidyverse)
require(magrittr)
require(pbapply)
require(ggplot2)

cache_folder <- file.path('data', '00_init', 'cache')
if(!dir.exists(cache_folder)) dir.create(cache_folder)

output_folder <- file.path('data', '00_init', 'output')
if(!dir.exists(output_folder)) dir.create(output_folder)


get_eea_meas <- function(){
  
    download_eea_meas_fresh <- function(years_force_refresh=NULL){
      base_url = 'https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?CountryCode=&CityName=&Pollutant=8&Year_from=2015&Year_to=2020&Station=&Samplingpoint=&Source=All&Output=TEXT&UpdateDate=&TimeCoverage=Year'
      
      pollutants = list(NO2=8, PM10=5, CO=10)
      urls = list()
      for(p in names(pollutants)) {
        base_url %>% gsub('Pollutant=8', paste0('Pollutant=', pollutants[[p]]), .) %>%
          gsub('Year_from=2015', paste0('Year_from=', 2015), .) %>% 
          gsub('Year_to=2020', paste0('Year_to=', 2020), .) %>% 
          readLines() -> urls[[p]]
      }
      
      urls %<>% unlist
      
      file_paths <-  file.path(cache_folder, gsub('.*/', '', urls))
      file_paths_to_download_i <- which(!file.exists(file_paths))
        
      if(!is.null(years_force_refresh)){
        file_paths_to_download_i <- unique(c(file_paths_to_download_i,
                                    grep(paste0('_',years_force_refresh,'_timeseries.csv',collapse='|'),
                                         file_paths, perl=T, value=F)))
      }

      url_to_download <- urls[file_paths_to_download_i]
      # Download
      for(u in urls) try(download.file(u, file.path(cache_folder, gsub('.*/', '', u))))
    }
    
    # build a data.frame of downloaded files
    # urls %>% gsub('.*/', '', .) -> inF
    inF <- list.files(path=cache_folder, pattern='*_timeseries.csv')
    inF <- do.call('rbind', lapply(inF, function(x){
        data.frame(t(strsplit(x, '_')[[1]][1:4])) %>%
          set_names(c('ISO3', 'PollutantCode', 'STID', 'Year')) %>%
          mutate(file=file.path(cache_folder,x)) %>% mutate_if(is.factor, as.character)
      }))
    
    # Read data
    coltypes <- inF$file[1] %>% read_csv() %>% sapply(class) %>% substr(1, 1)
    read_timeseries <- function(file){
      tryCatch({
        file %>% read_csv(col_types = coltypes, progress=F) %>% 
            filter(!is.na(Concentration), Concentration>0)},
        errorCondition(cond){
          NA
        })
    }
    inF <- inF %>% mutate(meas=list(map_dfr(file,read_timeseries)))
    return(inF)
}


meas <- readreadRDS(file.path('data','00_init','input','daily data all stations.RDS'))
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
