require(raster)

gadm1.get_sf <- function(iso2){
  iso3 <- countrycode::countrycode(unique(iso2), "iso2c", "iso3c")
  iso3 <- setdiff(iso3, "GIB")
  gadm1s <- lapply(iso3, function(x) tryCatch({raster::getData('GADM', country=x, level=1, path=cache_folder)},error=function(cond){NULL}))
  gadm1s <- gadm1s[!sapply(gadm1s,is.null)]
  gadm1 <- do.call(raster::bind, gadm1s) 
  gadm1_sf <- st_as_sf(gadm1) %>% dplyr::select(gadm0_id=GID_0, gadm1_id=GID_1, gadm1_name=NAME_1)
  return(gadm1_sf)
}