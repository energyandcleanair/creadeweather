require(purrr)
require(sf)
require(rnoaa)
require(dplyr)
require(worldmet)

#-----------------------------------------------------------------------------
# If no existing result: query the whole dataset again (can take 5-10 hours)
# If existing rsults: only query last year and merge into results
#-----------------------------------------------------------------------------
gadm1_sf <- sf::st_read('data/00_init/output/gadm1_filtered_bounds.geojson')
min_year=2015
max_year=2020
max_stations_per_gadm1=5



collect_weather_3h <- function(gadm1_sf, min_year, max_year, max_stations_per_gadm1){
  
  gadm1_noaa <- gadm1_sf %>%
    rowwise() %>%
    mutate(noaa_station=list(rnoaa::isd_stations_search(bbox = st_bbox(geometry))))
  
  gadm1_noaa <- gadm1_noaa %>%
    mutate(noaa_station_sample=list(noaa_station %>%
                                      arrange(desc(end), begin) %>%
                                      slice(1:max_stations_per_gadm1)))
  
  gadm1_weather_3h <- gadm1_noaa %>%
    dplyr::select(gadm1_id, gadm1_name, noaa_station_sample) %>%
    tidyr::unnest(cols=c(noaa_station_sample)) %>%
    mutate(weather = purrr::map2(.x=usaf, .y=wban,
                                 purrr::possibly(~worldmet::importNOAA(
                                   code = paste(.x, .y, sep="-"),
                                   year = min_year:max_year,
                                   hourly = FALSE,
                                   precip = TRUE,
                                   PWC = FALSE,
                                   parallel = T,
                                   quiet = FALSE,
                                   path = file.path(getwd(),'cache')
                                 ),
                                 otherwise = NA)))
  return(gadm1_weather_3h)
}

saveRDS(gadm1_weather_3h,file.path('data', '01_weather', 'tmp', 'gadm1_weather_3h.RDS')) #1GB

# Aggregate per day
gadm1_weather_day <- gadm1_weather_3h %>% filter(!is.na(weather)) %>%
  rowwise() %>% mutate(weather=list(
    weather %>%
      dplyr::group_by(lubridate::date(date)) %>%
      dplyr::summarize(
        air_temp=mean(air_temp, na.rm=T),
        atmos_pres=mean(atmos_pres, na.rm=T),
        wd=mean(wd, na.rm=T),
        ws=mean(ws, na.rm=T),
        ceil_hgt=mean(ceil_hgt, na.rm=T),
        visibility=mean(visibility, na.rm=T),
        precip=mean(precip, na.rm=T),
        RH=mean(RH, na.rm=T)
      )
  )
  )

# Aggregate per region
gadm1_weather_day_region <- gadm1_weather_day %>%
  rowwise() %>% mutate(weather=list(
    weather %>%
      dplyr::group_by(lubridate::date(date)) %>%
      dplyr::summarize(
        air_temp=mean(air_temp, na.rm=T),
        atmos_pres=mean(atmos_pres, na.rm=T),
        wd=mean(wd, na.rm=T),
        ws=mean(ws, na.rm=T),
        ceil_hgt=mean(ceil_hgt, na.rm=T),
        visibility=mean(visibility, na.rm=T),
        precip=mean(precip, na.rm=T),
        RH=mean(RH, na.rm=T)
      )
  )
  )
