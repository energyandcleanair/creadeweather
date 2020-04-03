require(dplyr)



lombardia.get_stations <- function(){
  file_metadata <- file.path(input_folder,'lombardia_stations.csv')
  if(!file.exists(file_metadata)){
    download.file(url='https://www.dati.lombardia.it/api/views/ib47-atvt/rows.csv?accessType=DOWNLOAD',
                  destfile=file_metadata)
  }
  
  stations <- read.csv(file_metadata) %>%
    dplyr::select(Idstazione, NomeTipoSensore, IdSensore, UnitaMisura, Idstazione, lat, lng) %>%
    mutate_if(is.factor, as.character) %>%
    rename(
      station_id=Idstazione, 
      pollutant=NomeTipoSensore,
      unit=UnitaMisura,
      sensor_id=IdSensore
    )
  
  poll_corr <- list(
    NO2="Biossido di Azoto",
    SO2="Biossido di Zolfo",
    O3="Ozono",
    CO="Monossido di Carbonio",
    BEN="Benzene",
    NO="Ossidi di Azoto",
    PM10="PM10 (SM2005)",
    PM25="Particelle sospese PM2.5",
    NH3="Ammoniaca",
    BC="BlackCarbon",
    NO="Monossido di Azoto"
  )
  
  poll_df <- unlist(poll_corr)
  stations$pollutant <- names(poll_df)[match(stations$pollutant, poll_df)]
  stations %<>% filter(!is.na(pollutant))
  return(stations)
}

lombardia.get_stations_sf <- function(){
  sf::st_as_sf(lombardia.get_stations(), coords=c("lng","lat"), crs=4326)
}

lombardia.get_meas <- function(pollutants_names){
  
  stations <- lombardia.get_stations()
  
  meas_url <- 'https://www.dati.lombardia.it/api/views/nicp-bhqi/rows.csv?accessType=DOWNLOAD'
  meas <- read.csv(url(meas_url))
  
  meas_cleaned <- meas %>%
    select(IdSensore, Data, Valore) %>% 
    rename(
      sensor_id=IdSensore,
      date=Data,
      value=Valore
    ) %>%
    filter(value>0) %>%
    mutate(date=lubridate::date(date)) %>%
    group_by(IdSensore, date) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    ungroup() %>%
    left_join(stations) %>%
    select(-c(sensor_id)) %>%
    group_by(station_id, pollutant, lat, lng) %>%
    tidyr::nest() %>%
    rename(meas=data)

  # Translating pollutant name
  unique(meas_cleaned$pollutant)
  meas_cleaned
}
