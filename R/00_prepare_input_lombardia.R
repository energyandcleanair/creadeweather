

lombardia.input_folder <- file.path('data', '00_init', 'input')

lombardia.get_stations <- function(){
  file_metadata <- file.path(lombardia.input_folder,'lombardia_stations.csv')
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

lombardia.get_meas <- function(){
  
  stations <- lombardia.get_stations()
  
  meas_url_2018 <- 'https://www.dati.lombardia.it/api/views/bgqm-yq56/rows.csv?accessType=DOWNLOAD'
  meas_url_2019 <- 'https://www.dati.lombardia.it/api/views/kujm-kavy/rows.csv?accessType=DOWNLOAD'
  meas_url_2020 <- 'https://www.dati.lombardia.it/api/views/nicp-bhqi/rows.csv?accessType=DOWNLOAD'
  meas_urls <- c(meas_url_2018, meas_url_2019, meas_url_2020)
  meas <- do.call('bind_rows', lapply(meas_urls, read.table, as.is=T, header=T, sep=","))

  meas_cleaned <- meas %>%
    select(IdSensore, Data, Valore) %>% 
    rename(
      sensor_id=IdSensore,
      date=Data,
      value=Valore
    ) %>%
    filter(value>0) %>%
    mutate(date=as.POSIXct(strptime(date,"%d/%m/%Y %H:%M:%S %p"))) %>%
    group_by(sensor_id, date) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    ungroup() %>%
    left_join(stations) %>%
    select(-c(sensor_id)) %>%
    group_by(station_id, pollutant, lat, lng) %>%
    tidyr::nest() %>%
    rename(meas=data)

  return(meas_cleaned)
}
