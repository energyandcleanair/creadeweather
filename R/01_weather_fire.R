
frp.cache.filename <- function(location_id, country, met_type, height, duration_hour, date){
  paste(tolower(country),
        tolower(location_id),
        gsub("\\.","",tolower(met_type)),
        height,
        duration_hour,
        gsub("-","",date),
        "RDS",
        sep=".")
}

frp.hysplit.trajs_at_date <- function(date, geometry, height, duration_hour, met_type){
  
  dir_hysplit_met <- Sys.getenv("DIR_HYSPLIT_MET", here::here(utils.get_cache_folder("weather")))
  dir_hysplit_output <-utils.get_cache_folder("trajs/output")
  lat <- sf::st_coordinates(geometry)[2]
  lon <- sf::st_coordinates(geometry)[1]
  
  tryCatch({
    trajs <- splitr::hysplit_trajectory(
      lon = lon,
      lat = lat,
      height = height,
      duration = duration_hour,
      days = lubridate::date(date),
      daily_hours = c(0, 6, 12, 18),
      direction = "backward",
      met_type = met_type,
      extended_met = F,
      met_dir = dir_hysplit_met,
      exec_dir = dir_hysplit_output,
      clean_up = T
    )
    
    # Update fields to be compatible with OpenAIR
    trajs$hour.inc <- trajs$hour_along
    trajs$date <- trajs$traj_dt_i
    trajs$date2 <- trajs$traj_dt
    trajs$year <- lubridate::year(trajs$traj_dt_i)
    trajs$month <- lubridate::month(trajs$traj_dt_i)
    trajs$day <- lubridate::date(trajs$traj_dt_i)
    
    return(trajs)
  },
  error=function(c){
    print(c)
    return(NA)
  })
  
}


# Trajectories ------------------------------------------------------------

frp.trajs_at_date <- function(date, location_id, geometry, country, met_type, height, duration_hour, ...){
  tryCatch({
    cache_folder <- utils.get_cache_folder("trajs")
    filename <- frp.cache.filename(location_id, country, met_type, height, duration_hour, date)
    filepath <- file.path(cache_folder, filename)
    
    if(!file.exists(filepath)){
      trajs <- frp.hysplit.trajs_at_date(date, geometry=geometry, met_type=met_type,
                                      duration_hour=duration_hour, height=height)
      if(!is.na(trajs)){
        saveRDS(trajs, filepath)
      }
    }else{
      trajs <- readRDS(filepath)
    }
    return(trajs)
  }, error=function(c){
    return(NA)
  })
  
}

frp.traj_extent <- function(traj, buffer_km){
  tryCatch({
    sf::st_as_sf(traj, coords=c("lon","lat"), crs=4326) %>%
      sf::st_transform(crs=3857) %>%
      sf::st_buffer(buffer_km*1000) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc()  
  }, error=function(c){
    return(NA)
  })
}

frp.union_extents <- function(l){
  do.call("st_union", as.list(l)) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()
}

# Fire radiative power ----------------------------------------------------
frp.utils.date_rasters <- function(rasters){
  result <- tibble()
  for(d in names(rasters)){
    date_init <- lubridate::date(names(rasters[d]))
    r <- raster::unstack(raster::stack(as.character(rasters[d])))
    dates <- seq(date_init, date_init+lubridate::days(length(r)-1), by="d")
    result <- bind_rows(result,
                        tibble(date=dates, raster=r))
  }
  return(result)
}


frp.average <- function(trajs.day, r, buffer_km){
  date <- lubridate::date(trajs.day$traj_dt) %>% unique()
  raster.day <- r %>% filter(date==!!date) %>% pull(raster)
  t <- st_union(st_buffer(st_transform(st_as_sf(trajs.day, coords=c("lon","lat"), crs=4326),crs=3857), buffer_km*1000))
  
  vs <- lapply(raster.day, function(x){
    max(0, x %>% exactextractr::exact_extract(t,fun="mean"), na.rm=T)
  })
  
  return(mean(unlist(vs)))
}

frp.frp_at_traj <- function(trajs, country, location_id, ..., frp.rasters, buffer_km){
  
  tryCatch({
    traj.sf <- sf::st_as_sf(trajs, coords=c("lon","lat"), crs=4326) %>%
      st_transform(crs=3857)
    
    r <- frp.rasters %>% filter(country==!!country, location_id==!!location_id)
    
    frp.day <- trajs %>%
      group_by(date=lubridate::date(traj_dt)) %>%
      tidyr::nest(trajs=!date) %>%
      rowwise() %>%
      mutate(frp=frp.average(trajs,r=r, buffer_km=buffer_km),
             count=nrow(trajs))
    
    return(weighted.mean(frp.day$frp, frp.day$count))  
  }, error=function(c){
    return(NA)
  })
  
}
# 
# frp.geotiffs.old <- function(date_from, date_to, extent, ...){
#   extent.sf <- sf::st_as_sf(sf::st_sfc(extent), crs=3857)
#   begin=format(date_from,"%Y%j")
#   end=format(date_to,"%Y%j")
#   rasters <- MODIS::runGdal(job="MOD14A1.day", product="MOD14A1", extent=extent.sf,
#                             begin=begin, end=end, SDSstring="0010",
#                             forceDownload=F)[[1]]
#   frp.utils.date_rasters(rasters)
# }

frp.geotiffs <- function(date_from, date_to, extent, ...){
  
  
  r <- MODIStsp::MODIStsp(
           gui = FALSE,
           start_date = format(lubridate::date(date_from)-lubridate::days(8), "%Y.%m.%d"), #Files are every 8 days. MODISstp doesn't find any file if inbetween
           end_date   = format(date_to,"%Y.%m.%d"),
           out_folder = file.path(Sys.getenv("DIR_MODIS"),"modisstp","processed"),
           out_folder_mod = file.path(Sys.getenv("DIR_MODIS"),"modisstp"),
           spatmeth = "tiles",
           # Tiles for India and Pakistan
           start_x = 23,
           end_x = 26,
           start_y = 5,
           end_y = 8,
           selcat = "Land Cover Characteristics - Thermal Anomalies and Fire",
           selprod = "ThermalAn_Fire_Daily_1Km (M*D14A1)",
           prod_version = "6",
           sensor = "Both",
           bandsel = "MaxFRP",
           download_server = "http",
           user = Sys.getenv("EARTHDATA_USR"),
           password = Sys.getenv("EARTHDATA_PWD"),
           downloader = "http",
           download_range = "Full",
           out_projsel = "Used Defined",
           output_proj = "3857",
           out_res_sel = "Native",
           out_res = NULL,
           resampling = "max",
           reprocess = F,
           delete_hdf = F,
           nodata_change = F,
           scale_val = F,
           out_format = "GTiff",
           ts_format = "R RasterStack",
           compress = "LZW",
           n_retries = 10,
           verbose = TRUE)
  
  
  frp.utils.date_rasters(rasters)
}

frp.add_frp <- function(weather, met_type="reanalysis", height=500, duration_hour=72, buffer_km=20){
  

  # Get trajectories - One row per day
  cols <- setdiff(c(names(weather), "date"),"weather")
  w <- tibble(weather) %>%
    tidyr::unnest(weather) %>%
    tidyr::nest(meas=!cols)
  
  wt <- w %>%
    rename(location_id=station_id) %>%
    mutate(trajs=purrr::pmap(., frp.trajs_at_date, met_type=met_type, height=height, duration_hour=duration_hour)) %>%
    mutate(trajs_extent=purrr::map(trajs, frp.traj_extent, buffer_km=buffer_km))
  
  # We're building one geotiff file per date per region
  # MODIS::MODISoptions(
  #   localArcPath=Sys.getenv("DIR_MODIS"),
  #   outDirPath=file.path(Sys.getenv("DIR_MODIS"),"processed"),
  #   dlmethod="wget",
  #   quiet=F)
  
  frp.rasters <- wt %>%
    group_by(country, location_id) %>% 
    summarise(extent=frp.union_extents(trajs_extent),
              date_from=min(date)-lubridate::hours(duration_hour),
              date_to=max(date)) %>%
    mutate(geotiffs=purrr::pmap(., frp.geotiffs)) %>%
    dplyr::select(country, location_id, geotiffs) %>%
    tidyr::unnest(geotiffs)
  
  # Calculate mean frp along trajectories
  wtf <- wt %>%
    mutate(frp=purrr::pmap_dbl(., frp.frp_at_traj, frp.rasters=frp.rasters, buffer_km=buffer_km))
  
  wtfd <- wtf %>%
    dplyr::select(country, location_id, date, frp) %>%
    dplyr::group_by(country, location_id) %>%
    tidyr::nest(frp=c(date,frp))
  
  # Adding frp to weather data
  result <- weather %>%
    left_join(wtfd) %>%
    rowwise() %>%
    mutate(weather=list(weather %>% left_join(frp)))
  
  return(result)
}
