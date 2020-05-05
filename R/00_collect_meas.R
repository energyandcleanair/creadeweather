#' Get stations and measurements in Europe for selected pollutants
#' and at selected geographical granularity from 2015 til now.
#'
#' @param pollutants a vector of pollutants e.g. creadb::NO2, creadb::PM10
#' @param years_force_refresh A vector of years for which we want to force downloading data
#' even when file is in cache folder. Useful if you want to get latest data of current year. If NULL,
#' no file is downloaded when it exists in cache folder.
#' @param deg latitude/longitude degree of the grid used to find station
#' @param country additional country filter (iso2)
#' @param cache_folder where to store cache files for measurements [optional]
#' @param output_folder where to store measurements RDS files [optional]
#' @param input_folder where to get additional input data from (not used at the moment) [optional]
#' @param filename output filename [optional]
#'
#' @return tibble of measurements
#' @export
#'
#' @examples collect_meas(pollutants=c(creadb::NO2, creadb::PM10), deg=0.2)
collect_meas <- function(pollutants,
                          years_force_refresh=NULL,
                          deg=0.2,
                          country=NULL,
                          cache_folder=file.path('data', '00_init', 'cache'),
                          output_folder=file.path('data', '00_init', 'output'),
                          input_folder=file.path('data', '00_init', 'input'),
                          filename=NULL
){
  
  if(!dir.exists(cache_folder)) dir.create(cache_folder, recursive = T)
  if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
  if(!dir.exists(input_folder)) dir.create(input_folder, recursive = T)
  
  # Get stations and regions
  stations_eea_sf <- eea.get_running_stations_sf(pollutants)
  countries <- if(is.null(country)) stations_eea_sf$iso2 else country
  continental_bbox <- sf::st_bbox(c(xmin = -13, 
                                    ymin = 25, 
                                    xmax = 50, 
                                    ymax = 70),
                                  crs = sf::st_crs(4326))
  stations_eea_sf <- st_intersection(stations_eea_sf, st_as_sfc(continental_bbox))
  stations_eea_present_sf <- stations_eea_sf 
  
  gadm1_sf <- gadm1.get_sf(countries, cache_folder=cache_folder)
  gadm1_sf$area <- st_area(gadm1_sf)
  gadm1_simplified_sf <- st_simplify(gadm1_sf,preserveTopology = T,dTolerance = 0.1)
  
  grid <- gadm1_simplified_sf %>% 
    st_make_grid(cellsize = deg, what = "centers") %>% # grid of points
    st_intersection(gadm1_simplified_sf)   
  
  nearest_stations_for_poll <- function(pollutant_){
    poll_stations <- stations_eea_present_sf %>% filter(pollutant==pollutant_)
    nearest_idx <- grid %>% st_nearest_feature(poll_stations)
    poll_stations[nearest_idx, ]
  }
  
  stations_eea_filtered <- do.call("rbind",lapply(pollutants, nearest_stations_for_poll))

  # Attach GADM1 to stations_eea
  stations_eea_gadm_sf <- st_join(stations_eea_sf, gadm1_sf)
  
  # We download first
  # we store references to accelerate later scraping / reading
  eea_refs <- tryCatch({
    readRDS(file.path(cache_folder,'eea_refs.RDS'))},
    error=function(err){
      tibble(station_id=character(),pollutant=character(),
             year=integer(),file=character(),url=character())
    }
  )
  
  eea_refs <- eea.download_stations_meas(stations=stations_eea_filtered %>%
                                           distinct(station_id, pollutant),
                                         years_force_refresh= years_force_refresh,
                                         refs=eea_refs,
                                         cache_folder=cache_folder
  )
  
  saveRDS(eea_refs, file.path(cache_folder,'eea_refs.RDS'))
  
  # read
  eea_meas <- eea.read_stations_meas(stations=stations_eea_filtered %>%
                                       distinct(station_id, pollutant),
                                     cache_folder=cache_folder)
  
  # nest
  eea_meas_nested <- eea_meas %>%
    group_by(station_id, pollutant, unit) %>%
    tidyr::nest() %>%
    rename(meas=data)
  
  # join GADM1 information
  eea_meas_nested <- eea_meas_nested %>%
    left_join(
      stations_eea_gadm_sf %>% dplyr::select(station_id, gadm0_id, gadm1_id, gadm1_name, geometry)
    )
  
  # save
  filename <- if(!is.null(filename)) filename else paste('eea_meas_daily',paste(tolower(pollutants),collapse='_'),sub('\\.','',deg),'.RDS',sep='_')
  saveRDS(eea_meas_nested, file.path(output_folder,filename))
  return(eea_meas_nested)
}
