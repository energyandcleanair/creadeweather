require(purrr)
require(sf)
require(raster)
require(tidyverse)
require(magrittr)
require(ggplot2)
require(countrycode)
require(lwgeom)
require(pbapply)
require(stringr)

eea.get_running_stations <- function(pollutants){
  
  urls_latest <- readLines('http://discomap.eea.europa.eu/map/fme/latest/files.txt') %>% trimws()
  urls_latest_poll <- grep(paste0('_',paste0(pollutants, collapse="|"),'.csv'), urls_latest, value=T)
  parse_url <- function(u){
    tryCatch({
      read.csv(url(URLencode(u))) %>%
        dplyr::distinct(network_countrycode, station_localid, samplingpoint_x, samplingpoint_y) %>%
        dplyr::rename(iso2=network_countrycode, station_id=station_localid, latitude=samplingpoint_y, longitude=samplingpoint_x)  %>%
        dplyr::mutate(pollutant=str_match(u,"_([:alnum:]+).csv")[,2]) # Attaching pollutant name
    },error=function(err) NULL)
  }
  
  do.call('bind_rows', pblapply(urls_latest_poll, parse_url)) %>% distinct(iso2, station_id, pollutant, latitude, longitude)
  
}

eea.get_stations <- function(){
  file_metadata <- file.path(input_folder,'PanEuropean_metadata.csv')
  if(!file.exists(file_metadata)){
    download.file(url='http://discomap.eea.europa.eu/map/fme/metadata/PanEuropean_metadata.csv',
                  destfile=file_metadata)
  }
  stations <- read.csv(file_metadata, header=T, sep="\t") %>%
    distinct(Countrycode, AirQualityStation, Latitude, Longitude, ObservationDateBegin, ObservationDateEnd) %>%
    rename(
      iso2=Countrycode,
      station_id=AirQualityStation,
      latitude=Latitude,
      longitude=Longitude,
      date_from=ObservationDateBegin,
      date_to=ObservationDateEnd) %>%
    mutate_if(is.factor, as.character) %>%
    group_by(iso2, station_id) %>%
    summarise(
      latitude=first(latitude),
      longitude=first(longitude),
      date_from=min(date_from),
      date_to=if('' %in% date_to) 'present' else max(date_to)
    ) 
  return(stations)
}



eea.get_stations_sf <- function(){
  sf::st_as_sf(eea.get_stations(), coords=c("longitude","latitude"), crs=4326)
}

eea.get_running_stations_sf <- function(pollutants){
  sf::st_as_sf(eea.get_running_stations(pollutants), coords=c("longitude","latitude"), crs=4326)
}

eea.download_station_meas <- function(station_id, pollutant_names, years_force_refresh=c(2020), refs, cache_folder){
  print(station_id)  
  tryCatch({
    station_id_=station_id
    base_url = 'https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?CountryCode=&CityName=&Pollutant=8&Year_from=2015&Year_to=2020&Station=&Samplingpoint=&Source=All&Output=TEXT&UpdateDate=&TimeCoverage=Year'
    
    pollutants = list(NO2=8, PM10=5, CO=10, SO2=1, O3=7)
    
    urls = list()
    for(p in pollutant_names) {
      
      # Check if exist already
      existing_refs <- refs %>% filter(station_id==station_id_, pollutant==p, year %in% c(2015:2020))
      files <- existing_refs$file
      
      if(length(files)==0){
        base_url %>% gsub('Pollutant=8', paste0('Pollutant=', pollutants[[p]]), .) %>%
          gsub('Station=', paste0('Station=', station_id), .) %>%
          gsub('Year_from=2015', paste0('Year_from=', 2015), .) %>% 
          gsub('Year_to=2020', paste0('Year_to=', 2020), .) %>% 
          readLines() -> urls[[p]]
        
        files <- gsub('.*/', '', urls[[p]])
        pat = '_(\\d{4})_timeseries'
        years <- as.integer(str_match(files, pat)[, 2])
        
        refs <- refs %>% bind_rows(tibble(station_id=station_id,
                                          pollutant=p,
                                          year=years,
                                          file=files,
                                          url=urls[[p]]))
      }else{
        urls[[p]] <- existing_refs$url
      }
    }
    
    urls %<>% unlist
    files <- gsub('.*/', '', urls)
    meas <- do.call('rbind', lapply(files, function(x){
      data.frame(t(strsplit(x, '_')[[1]][1:4])) %>%
        set_names(c('iso2', 'pollutant', 'station_id', 'year')) %>%
        mutate(file=file.path(cache_folder,x)) %>% mutate_if(is.factor, as.character)
    }))
    
    # rename pollutant
    pollutants <- unlist(pollutants)
    meas$pollutant <- names(pollutants)[match(meas$pollutant, pollutants)]
    
    # adding urls
    meas$url <- urls
    
    # download
    file_paths <-  meas$file
    if(is.null(file_paths)){
      print('Empty')
      return(refs)
    }
    # Find files to download
    file_paths_to_download_i <- which(!file.exists(file_paths))
    if(!is.null(years_force_refresh)){
      file_paths_to_download_i <- unique(c(file_paths_to_download_i,
                                           grep(paste0('_',years_force_refresh,'_timeseries.csv',collapse='|'),
                                                file_paths, perl=T, value=F)))
    }
    url_to_download <- meas$url[file_paths_to_download_i]
    
    # Download files
    lapply(url_to_download,
           function(u) try(download.file(u, file.path(cache_folder, gsub('.*/', '', u))))
    )
    
    return(refs)
  }, error=function(err) refs
  )
}

eea.download_stations_meas <- function(stations, years_force_refresh=c(2020), refs, cache_folder){
  pb <- txtProgressBar(min = 0, max = nrow(stations), style = 3)
  i <- 0
  
  for(i in seq(nrow(stations))){
    refs<-eea.download_station_meas(stations[i,]$station_id,
                                    pollutant_names=stations[i,]$pollutant,
                                    years_force_refresh=years_force_refresh,
                                    refs,
                                    cache_folder=cache_folder)
    i <- i+1
    setTxtProgressBar(pb, i)
  }
  
  return(refs)
}

eea.read_stations_meas <- function(stations, cache_folder, years_force_refresh=NULL){
  
  file_paths <- list.files(cache_folder,'*_timeseries.csv', full.names = T)
  
  # We open every single file ancd check it belong s to station_ids & pollutant
  
  filter_file <- function(f, stations){
    tryCatch({
      fl <- read_csv(f, n_max = 1, progress=F, col_types = cols())
      res <- (fl$AirQualityStation %in% stations$station_id) &&
              (fl$AirPollutant %in% stations[stations$station_id==fl$AirQualityStation,'pollutant'])
      if(is.na(res) || is.null(res)) FALSE else res
    }, error=function(cond){FALSE}
    )
  }
  
  read_file <- function(f){
    meas <- f %>% read_csv(progress=F, col_types = cols()) %>% 
      dplyr::filter(!is.na(Concentration), Concentration>0) %>%
      dplyr::select(Countrycode, AirQualityStation, AirPollutant, Concentration, UnitOfMeasurement, DatetimeBegin, DatetimeEnd) %>%
      rename(
        iso2=Countrycode,
        station_id=AirQualityStation,
        pollutant=AirPollutant,
        value=Concentration,
        unit=UnitOfMeasurement,
        date_from=DatetimeBegin) %>%
      dplyr::mutate(date=lubridate::date(date_from)) %>% # Group by day
      dplyr::group_by(iso2, station_id, pollutant, date, unit) %>%
      dplyr::summarise(value=mean(value, na.rm=T))
  }
  
  filter_and_read_file <- function(f, stations){
    if(filter_file(f, stations)) read_file(f) else NULL
  }
  
  bind_rows(pblapply(file_paths, filter_and_read_file, stations=stations))
}
