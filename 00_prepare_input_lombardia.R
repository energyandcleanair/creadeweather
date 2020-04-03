

lombardia.get_meas <- function(pollutants_names){
  stations_url <- 'https://www.dati.lombardia.it/api/views/ib47-atvt/rows.csv?accessType=DOWNLOAD'
  stations <- read.csv(url(stations_url)) %>% select(Idstazione, NomeTipoSensore, IdSensore, UnitaMisura, Idstazione, lat, lng)
  
  meas_url <- 'https://www.dati.lombardia.it/api/views/nicp-bhqi/rows.csv?accessType=DOWNLOAD'
  meas <- read.csv(url(meas_url))
  
  meas_cleaned <- meas %>%
    select(IdSensore, Data, Valore) %>% 
    rename(
      date=Data,
      value=Valore
    ) %>%
    filter(value>0) %>%
    mutate(date=lubridate::date(date)) %>%
    group_by(IdSensore, date) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    ungroup() %>%
    left_join(stations) %>%
    rename(
      station_id=Idstazione, 
      pollutant=NomeTipoSensore,
      unit=UnitaMisura
    ) %>%
    select(-c(IdSensore)) %>%
    group_by(station_id, pollutant, lat, lng) %>%
    tidyr::nest() %>%
    rename(meas=data)

  # Translating pollutant name
  unique(meas_cleaned$pollutant)
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
  meas_cleaned$pollutant <- names(poll_df)[match(meas_cleaned$pollutant, poll_df)]
  return(meas_cleaned %>% filter(pollutant %in% pollutants_names))
}