require(jsonlite)
require(dplyr)
require(countrycode)
require(sf)
devtools::install_github("https://github.com/energyandcleanair/rcrea", force=T)
require(rcrea)

##################
# 0. Parameters
##################
iso3s <- c("ITA", "FRA", "BEL", "GER", "GBR") # Null if want all of them (not tested yet)
cache_folder <- "data/00_init/cache" # To store GADM shapefiles
source <- "eea" # In Europe, eea is recommended. India: cpcb Elsewhere: openaq
polls <- c(rcrea::NO2, rcrea::PM25)


#####################
# 1. Build datasets
#####################

# 1.1 Get covid data
covid_data <- tibble(read.csv("https://coronadatascraper.com/timeseries.csv"))
covid_locations <- tibble(fromJSON("https://coronadatascraper.com/locations.json")) %>%
  mutate(iso3=countrycode::countrycode(country,"country.name","iso3c"),
         gadm_level=recode(level,"city"=NULL,"county"="2","state"="1","country"="0"))

# 1.2 Match covid locations with air pollutant ones (using iso2 -> GADM)
covid_data <- covid_data %>%
  mutate(iso3=countrycode::countrycode(country,"country.name","iso3c"),
         iso2=countrycode::countrycode(country,"country.name","iso2c"),
      aggregate_level=recode(level,"city"="city","county"="gadm2","state"="gadm1","country"="country"),
         gadm_level=recode(level,"city"=NULL,"county"="2","state"="1","country"="0"))

gadm <- covid_data %>% 
  filter(iso3 %in% iso3s) %>%
  distinct(iso3, gadm_level) %>%
  rowwise() %>%
  mutate(gadm=list(raster::getData('GADM',
                                   country=iso3,
                                   path=cache_folder,
                                   level=as.numeric(gadm_level))))

gadm_id <- function(iso3_, gadm_level_, coordinates_){
  tryCatch({
    pt <- st_as_sfc(paste0("POINT(",paste0(coordinates_, collapse=" "),")"), crs=4326) %>% st_sf()
    g <- st_join(pt, st_as_sf((gadm %>% filter(iso3==iso3_, gadm_level==gadm_level_))$gadm[[1]]), join = st_intersects)
    
    return(switch(as.character(gadm_level_),
                  "0"= g$GID_0,
                  "1"= g$GID_1,
                  "2"= g$GID_2))  
  }, error=function(err){
    warning(err)
    NA
  })
}

covid_locations <- covid_locations %>% filter(iso3 %in% iso3s) %>% rowwise() %>%
  mutate(gadm_id=gadm_id(iso3, gadm_level, coordinates))

# 1.3 Get air quality measurements in these locations
meas <- bind_rows(
  rcrea::measurements(location_id=covid_locations %>% filter(gadm_level==1) %>% dplyr::pull(gadm_id),
                      aggregate_level="gadm1", date_from="2020-01-01", source=source, poll=polls),
  rcrea::measurements(location_id=covid_locations %>% filter(gadm_level==2) %>% dplyr::pull(gadm_id),
                      aggregate_level="gadm2", date_from="2020-01-01",source=source, poll=polls)
) %>%
  mutate(date=lubridate::date(date)) %>%
  tidyr::spread(poll, value)

# 1.4 Merging covid and aq data
covid_meas_data <- covid_data %>% merge(covid_locations %>% dplyr::select(name, gadm_id),
                     by="name") %>% mutate(date=lubridate::date(as.character(date)),
                                           gadm_id=tolower(gadm_id))

covid_meas_data <- covid_meas_data %>% full_join(meas, by=c("gadm_id"="region_id","date"="date"))

