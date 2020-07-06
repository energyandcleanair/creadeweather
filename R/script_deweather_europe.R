#' Deweathering data in European main cities
#'
#' @return
#' @export
#'
#' @examples
scripts.deweather_europe <- function(){
  
  #-------------------------
  # Get cities of interest
  #-------------------------
  # Cities of interest (only those with measurements will ultimately be kept)
  # Getting capitals or cities with a population larger than 1M
  cities <- maps::world.cities %>%
    filter(capital==1 | pop > 1e6) %>%
    mutate(iso2=countrycode::countrycode(country.etc, "country.name","iso2c"),
           iso3=countrycode::countrycode(country.etc, "country.name","iso3c"),
           eu28=countrycode::countrycode(country.etc, "country.name","eu28"),
    ) %>%
    filter(!is.na(eu28))
  
  #------------------------------------
  # Find stations within these cities
  #------------------------------------
  
  # Get CREA db stations
  # locs <- rcrea::locations(source="eea")
  # 
  # # Attach city to stations
  # locs_cities <- utils.attach_city(locs, method="name", cities=cities)
  # 
  # locs_cities <- locs_cities[!is.na(locs_cities$name),]
  # locs_cities[locs_cities$name=="Budapesti",]$name<- "Budapest"
  # locs_cities[locs_cities$name=="Milano",]$name<- "Milan"
  # locs_cities[locs_cities$name=="Vilniaus",]$name<- "Vilnius"
  # locs_cities[locs_cities$name=="Roma",]$name<- "Rome"
  # locs_cities[locs_cities$name=="Lisboa",]$name<- "Lisbon"
  
  # print(locs_cities %>% group_by(name) %>%
  #         summarise(count=dplyr::n()))
  
  # results <- deweather(
  #   polls=c(rcrea::NO2),
  #   source=c("eea"),
  #   station_id=locs_cities$id,
  #   aggregate_level="station",
  #   output="anomaly",
  #   upload_results=T
  # )
  
  results <- deweather(
    poll=c(rcrea::NO2),
    source=c("eea"),
    station_id=cities$name,
    aggregate_level="city",
    output="anomaly_offsetted",
    upload_results=T
  )
}


