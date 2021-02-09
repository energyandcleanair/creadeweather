# Attaching fire information
# 2 x 2  options
# Using active fires points vs fire radiative power raster
# Using a buffer around geometry vs a buffer around trajectories (more time consuming)


#' Add fire related information to weather data
#'
#' @param weather the dataframe to add fire information to
#' @param activefire_or_raster should we use FIRMS active fire data or M*D14A1 maximum frp. "activefire" or "raster"
#' @param mode should we consider a simple buffer around geometry, a wind oriented buffer, or calculate trajectories.
#' attach_mode can be "circular", "oriented" or "trajectory" (oriented not working yet)
#' @param buffer_km buffer around geometry or trajectories. Default 200km for `circular` and `oriented`, 20km for `trajectory`
#' @param met_type met_type used for hysplit (if mode=="trajectory")
#' @param height height used for hysplit (if mode=="trajectory")
#' @param duration_hour duration in hours used for hysplit (if mode=="trajectory")

#'
#' @return
#' @export
#'
#' @examples
frp.add_frp <- function(weather,
                        activefire_or_raster="activefire",
                        mode="oriented",
                        met_type="gdas1",
                        height=500, 
                        duration_hour=72,
                        buffer_km=switch(mode,"circular"=200, "oriented"=200, "trajectory"=100)){
  
  
  # Get trajectories - One row per day
  cols <- setdiff(c(names(weather), "date", "wd_copy", "ws_copy"),"weather")
  w <- tibble(weather) %>%
    tidyr::unnest(weather) %>%
    # We keep wind information in case mode=='oriented'
    mutate(wd_copy=wd,ws_copy=ws) %>%
    tidyr::nest(meas=!cols)
  
  
  if(mode=="trajectory"){
    print("Calculating trajs")
    wt <- w %>%
      group_by(station_id) %>%
      summarise(trajs=list(frp.trajs_at_dates(date, unique(station_id), unique(geometry)[[1]],
                                              met_type=met_type,
                                              height=height,
                                              duration_hour=duration_hour))) %>%
      tidyr::unnest(trajs) %>%
      mutate(date_meas=lubridate::date(traj_dt_i)) %>%
      mutate(date_fire=lubridate::date(traj_dt)) %>%
      tidyr::nest(trajs=-c(station_id, date_meas, date_fire)) %>%
      rowwise() %>%
      mutate(extent=frp.traj_extent(trajs=trajs, buffer_km=buffer_km))
    
    one_extent_per_date <- T
  }else if(mode=="oriented"){
    #TODO Still quite slow
    print("Calculating extents")
    wt <- w %>%
      rowwise() %>%
      rename(date_meas=date) %>%
      mutate(extent=frp.oriented_extent(geometry,
                                        duration_hour=duration_hour,
                                        ws=ws_copy,
                                        wd=wd_copy)) %>%
      rowwise() %>%
      mutate(date_fire=list(seq(lubridate::date(date_meas) - lubridate::hours(duration_hour),
                                lubridate::date(date_meas),
                                by="day"))) %>%
      tidyr::unnest(date_fire)
    
    one_extent_per_date <- T
  }else if(mode=="circular"){
    wt <- w %>%
      distinct(station_id, geometry) %>%
      mutate(extent=frp.circular_extent(geometry,buffer_km=buffer_km)) %>%
      left_join(w) %>%
      rename(date_meas=date) %>%
      rowwise() %>%
      mutate(date_fire=list(seq(lubridate::date(date_meas) - lubridate::hours(duration_hour),
                           lubridate::date(date_meas),
                           by="day"))) %>%
      tidyr::unnest(date_fire)

    one_extent_per_date <- F
  }
  
  if(activefire_or_raster=="activefire"){
    # Download active fires
    frp.active.download(date_from=min(wt$date_fire),
                        date_to=max(wt$date_fire))
    
    
    # Add
    wtf <- frp.active.attach(wt,
                             one_extent_per_date=one_extent_per_date,
                             duration_hour=duration_hour)
    
    # Adding frp to weather data
    result <- weather %>%
      left_join(wtf %>%
                  dplyr::select(station_id, date, fire_frp, fire_count) %>%
                  tidyr::nest(frp=-c(station_id))) %>%
      rowwise() %>%
      mutate(weather=list(weather %>% left_join(frp))) %>%
      dplyr::select(-c(frp))
    
    return(result)
    
    
  }else if(activefire_or_raster=="raster"){
    
    # We're building one geotiff file per date per region
    # MODIS::MODISoptions(
    #   localArcPath=Sys.getenv("DIR_MODIS"),
    #   outDirPath=file.path(Sys.getenv("DIR_MODIS"),"processed"),
    #   dlmethod="wget",
    #   quiet=F)
    
    # 
    # frp.rasters <- wt %>%
    #   group_by(country, location_id) %>% 
    #   summarise(extent=frp.union_extents(trajs_extent),
    #             date_from=min(date)-lubridate::hours(duration_hour),
    #             date_to=max(date)) %>%
    #   mutate(geotiffs=purrr::pmap(., frp.geotiffs)) %>%
    #   dplyr::select(country, location_id, geotiffs) %>%
    #   tidyr::unnest(geotiffs)
    # 
    # 
    # # Calculate mean frp along trajectories
    # wtf <- wt %>%
    #   mutate(frp=purrr::pmap_dbl(., frp.frp_at_traj, frp.rasters=frp.rasters, buffer_km=buffer_km))
    # 
    # wtfd <- wtf %>%
    #   dplyr::select(country, location_id, date, frp) %>%
    #   dplyr::group_by(country, location_id) %>%
    #   tidyr::nest(frp=c(date,frp))
    # 
    # # Adding frp to weather data
    # result <- weather %>%
    #   left_join(wtfd) %>%
    #   rowwise() %>%
    #   mutate(weather=list(weather %>% left_join(frp)))
    return(weather)
  }
}


# Using Active Fires points (FIRMS)
frp.active.get_firms_folder <- function(){
  suppressWarnings(readRenviron(".Renviron"))
  suppressWarnings(readRenviron(".env"))
  
  d <- Sys.getenv("DIR_FIRMS")
  if(d==""){
    stop("DIR_FIRMS environment variable not defined")
  }
  return(d)
}

frp.active.get_firms_subfolder <- function(region="Global"){
  d <- frp.active.get_firms_folder()
  return(file.path(d, "suomi-npp-viirs-c2", region))
}

frp.active.download <- function(date_from=NULL, date_to=NULL, region="Global"){
  d <- file.path(frp.active.get_firms_folder(),
                 "suomi-npp-viirs-c2",
                 "Global")
  dir.create(d, showWarnings = F, recursive = T)
  
  date_from <- max(lubridate::date(date_from), lubridate::date("2020-01-01"))
  # Data not available in repository before that
  # Have been downloaded manually prior 2020
  # https://firms.modaps.eosdis.nasa.gov/download/
  if(date_to >= date_from){
    files.all <- paste0("SUOMI_VIIRS_C2_",
                        region,
                        "_VNP14IMGTDL_NRT_",
                        as.POSIXct(seq(lubridate::date(date_from),
                                       lubridate::date(date_to),
                                       by="day")) %>% strftime("%Y%j"),
                        ".txt")
    
    # Even though wget can manage already downloaded files,
    # this is faster and less verbose in console
    files.existing <- list.files(d, "*.txt")
    file.todownload <- setdiff(files.all, files.existing)
    
    modis_key <- Sys.getenv("MODIS_KEY")
    for(f in file.todownload){
      cmd <- paste0("wget -e robots=off -nc -np -R .html,.tmp -nH --cut-dirs=5 \"https://nrt3.modaps.eosdis.nasa.gov/api/v2/content/archives/FIRMS/suomi-npp-viirs-c2/", region, "/", f,"\" --header \"Authorization: Bearer ",
                    modis_key,
                    "\" -P ",
                    d)
      system(cmd)
    }
  }
}


frp.active.read <- function(date_from=NULL, date_to=NULL, region="Global", extent.sp=NULL){
  
  d <- frp.active.get_firms_subfolder(region=region)
  
  files_nrt <- list.files(d, paste0("SUOMI_VIIRS_C2_",region,"_VNP14IMGTDL_NRT_(\\d*).txt"), full.names = T) #TODO Further filtering
  files_archive <- list.files(d, "fire_.*[0-9]{7}.txt", full.names = T)
  files <- c(files_nrt, files_archive)
  
  f_min_date <- function(f, date_from, date_to){
    # ifelse(
    as.POSIXct(gsub(".*([0-9]{7})\\.(txt|csv)","\\1", f), format="%Y%j")
    # Yearly files have been manually spread into smaller daily ones
    # as.POSIXct(gsub(".*([0-9]{4})\\.(txt|csv)","\\1-01-01", f), format="%Y-%m-%d")
    # )
  }
  
  f_max_date <- function(f, date_from, date_to){
    # ifelse(
    # stringr::str_detect(f, "[0-9]{7}"),
    as.POSIXct(gsub(".*([0-9]{7})\\.(txt|csv)","\\1", f), format="%Y%j")
    # Yearly files have been manually spread into smaller daily ones
    # as.POSIXct(gsub(".*([0-9]{4})\\.(txt|csv)","\\1-12-31", f), format="%Y-%m-%d")
    # )
  }
  
  files <- files[is.null(date_from) | (f_max_date(files) >= as.POSIXct(date_from))]
  files <- files[is.null(date_to) | (f_min_date(files) <= as.POSIXct(date_to))]
  
  read.csv.fire <-function(f){
    tryCatch({
      # sp::over so much faster than sf (~5x)
      # fread vs read.csv also saves a lot of time
      d <- data.table::fread(f,
                             stringsAsFactors = F,
                             colClasses = c("acq_time"="character",
                                            "version"="character"))[,c("latitude","longitude","acq_date","frp")]
      d.coords <- cbind(d$longitude, d$latitude)
      df <- sp::SpatialPointsDataFrame(d.coords, d, proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))[c("acq_date","frp")]
      sp::proj4string(extent.sp) <- sp::proj4string(df)
      
      df[!is.na(sp::over(df, extent.sp)),] %>%
        sf::st_as_sf() # To allow bind_rows (vs rbind)
    }, error=function(c){
      warning(paste("Failed reading file", f))
      message(c)
      return(NULL)
    })
  }
  
  fires <- do.call("bind_rows",
                   pbmcapply::pbmclapply(sort(files[!is.na(files)]), # Sort to read fire_global* first
                                         read.csv.fire,
                                         mc.cores = parallel::detectCores()-1))
  fires
}

frp.active.relevant_fires_summary <- function(date, extent, duration_hour, f.sf){
  extent.sf <- sf::st_sfc(extent)
  sf::st_crs(extent.sf) <- sf::st_crs(f.sf)
  
  suppressMessages(f.sf %>%
                     filter(acq_date <= date, acq_date>=lubridate::date(date) - lubridate::hours(duration_hour)) %>%
                     filter(nrow(.)>0 & sf::st_intersects(., extent.sf, sparse = F)) %>%
                     as.data.frame() %>%
                     group_by() %>%
                     summarise(fire_frp=sum(frp, na.rm=T),
                               fire_count=dplyr::n()))
}

frp.active.attach <- function(wt, one_extent_per_date=T, duration_hour=72){
  
  extent.sp <- sf::as_Spatial(wt$extent[!sf::st_is_empty(wt$extent)])
  # extent.sp.union <- rgeos::gUnaryUnion(extent.sp)
  
  # Read and only keep fires within extent to save memory
  print("Reading fire files")
  f.sf <- frp.active.read(date_from=min(wt$date_fire),
                          date_to=max(wt$date_fire),
                          extent.sp=extent.sp)
  print("Done")
  
  if(nrow(f.sf)==0){
    print("DEBUG0")
    return(wt %>% mutate(fires=NULL))
  }
  
  if(one_extent_per_date){
    # Much slower, but required
    # We do one summary per row
    print("DEBUG1")
    regions <- wt %>% distinct(station_id, date_meas, date_fire, extent)
    
    
    
    print("Attaching fires to stations and dates")
    regions$fires <- pbapply::pbmapply(frp.active.relevant_fires_summary,
                                       date=regions$date_fire,
                                       extent=regions$extent,
                                       duration_hour=duration_hour,
                                       f.sf=list(f.sf),
                                       SIMPLIFY=F)
    #mc.cores = 1) # Too memory intensive otherwise -> killed ?
    
    
    wtf <- wt %>% left_join(regions %>%
                              tidyr::unnest(fires) %>%
                              select(-c(extent))) %>%
      select(-c(extent)) %>%
      group_by(station_id, date=date_meas) %>%
      # If you want to give more weight to 
      # Close fires, that'd be here probably
      summarise(fire_frp=sum(fire_frp),
                fire_count=sum(fire_count))
    
    print("Done")
    
  }else{
    print("DEBUG3")
    regions <- wt %>% distinct(station_id, extent)
    
    
    # We summarize before joining, less flexible but faster
    f.regions <- regions %>% sf::st_as_sf() %>%
      sf::st_join(f.sf, join=sf::st_contains) %>%
      tibble() %>%
      group_by(station_id, date_fire=acq_date) %>%
      summarise(frp=sum(frp, na.rm=T),
                fire_count=dplyr::n())
    
    # wt$date_from <- wt$date_meas - lubridate::hours(x=duration_hour)
    
    wtf <- wt %>% dplyr::left_join(
      f.regions %>% mutate(date_fire=lubridate::date(date_fire)),
      by = c("station_id", "date_fire")
    ) %>%
      group_by(station_id, date_meas) %>%
      summarise(fire_frp=sum(frp, na.rm=T),
                fire_count=sum(fire_count, na.rm=T))
    
  }
  
  return(wtf)
}


# Using Raster ------------------------------------------------------------
frp.trajs.cache_filename <- function(location_id, country, met_type, height, duration_hour, date){
  paste(tolower(country),
        tolower(location_id),
        gsub("\\.","",tolower(met_type)),
        height,
        duration_hour,
        gsub("-","",date),
        "RDS",
        sep=".")
}

frp.hysplit.trajs_at_dates <- function(dates, geometry, height, duration_hour, met_type){
  
  dir_hysplit_met <- Sys.getenv("DIR_HYSPLIT_MET", here::here(utils.get_cache_folder("weather")))
  dir_hysplit_output <-utils.get_cache_folder("trajs/output")
  lat <- sf::st_coordinates(geometry)[2]
  lon <- sf::st_coordinates(geometry)[1]
  
  tryCatch({
    # Probably slower to do date by date,
    # But facing unknown issue when too many dates at a time
    # Probably if one fails, all fail
    trajs <- do.call("bind_rows",
                     lapply(dates,
                            function(date){
                              splitr::hysplit_trajectory(
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
                              )}))
    
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
frp.cache.filename <- function(location_id, country, met_type, height, duration_hour, date){
  paste(location_id, met_type, height, duration_hour, strftime(date, "%Y%m%d"),"RDS",
        sep=".")
}


frp.trajs_at_dates <- function(dates, location_id, geometry, country, met_type, height, duration_hour, ...){
  
  tryCatch({
    cache_folder <- utils.get_cache_folder("trajs")
    print(paste0("Calculating trajs for ", location_id,". Storing in ", cache_folder))
    filenames <- frp.cache.filename(location_id, country, met_type, height, duration_hour, dates)
    filepaths <- file.path(cache_folder, filenames)
    
    filepaths.existing <- filepaths[file.exists(filepaths)]
    filepaths.missing <- filepaths[!file.exists(filepaths)]
    dates.missing <- dates[!file.exists(filepaths)]
    
    trajs <- do.call("bind_rows",
                     lapply(filepaths.existing, readRDS))
    
    if(length(dates.missing)>0){
      trajs.missing <- frp.hysplit.trajs_at_dates(dates.missing, geometry=geometry, met_type=met_type,
                                                  duration_hour=duration_hour, height=height)
      
      trajs.missing.days <- split(trajs.missing, lubridate::date(trajs.missing$traj_dt_i))
      lapply(names(trajs.missing.days),
             function(date){
               t <- trajs.missing.days[[date]]
               f <- file.path(cache_folder,
                              frp.cache.filename(location_id, country, met_type, height, duration_hour, date))
               saveRDS(t,f)
             })
      
      trajs <- bind_rows(trajs,
                         trajs.missing)
    }
    
    return(trajs)
  }, error=function(c){
    return(NA)
  })
  
}

frp.traj_extent <- function(trajs, buffer_km){
  tryCatch({
    suppressMessages(sf::st_as_sf(trajs, coords=c("lon","lat"), crs=4326) %>%
      group_by(run) %>%
      summarise() %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_transform(crs=3857) %>%
      sf::st_buffer(buffer_km*1000) %>%
      # sf::st_bbox() %>%
      # sf::st_as_sfc() %>%
      sf::st_transform(crs=4326) %>%
      sf::st_union())
      
  }, error=function(c){
    return(NA)
  })
}

frp.circular_extent <- function(geometry, buffer_km){
  tryCatch({
    sf::st_sfc(geometry, crs=4326) %>%
      sf::st_transform(crs=3857) %>%
      sf::st_buffer(buffer_km*1000) %>%
      sf::st_transform(crs=4326)
    # sf::st_bbox() %>%
    # sf::st_as_sfc()  
  }, error=function(c){
    return(NA)
  })
}

#' Generate a "pie slice" of with a certain angle, coming
#' from wind direction. Using wind_speed * duration to determine
#' radius of the pie slice.
#'
#' @param geometry 
#' @param buffer_km 
#' @param wd The angle, measured in a clockwise direction, between true north and the direction from which
#' the wind is blowing (wd from ISD). Calm winds if wd==0
#'
#' @return
#' @export
#'
#' @examples
frp.oriented_extent <- function(geometry, duration_hour, ws, wd, width_deg=90,
                                default_buffer_km=200,
                                max_buffer_km=800){
  
  st_wedge <- function(x, y, r, wd, width_deg, distance_km, n=20){
    if(wd==0){
      # Calm winds. We do a full circle, but reduce its buffer
      # so that area covered is the same
      n=n*360/width_deg
      distance_km = distance_km * sqrt(width_deg/360)
      width_deg=360
    }
    
    theta = seq(wd+180-width_deg/2, wd+180+width_deg/2, length=n) * pi/180  
    xarc = x + distance_km*1000*sin(theta)
    yarc = y + distance_km*1000*cos(theta)
    xc = c(x, xarc, x)
    yc = c(y, yarc, y)
    sf::st_polygon(list(cbind(xc,yc)))
  }
  
  tryCatch({
    # ws in m per second * 10
    distance_km <- min(ws * 3600/1000 * duration_hour / 10, max_buffer_km)
    if(is.na(distance_km)){
      # Probably missing wind speed
      distance_km <- default_buffer_km
    }
    
    sf::st_sfc(geometry, crs=4326) %>%
      sf::st_transform(crs=3857) %>%
      st_wedge(x=sf::st_coordinates(.)[1],
               y=sf::st_coordinates(.)[2],
               wd=wd,
               # ws is in meters per second * 10)
               distance_km = distance_km,
               width_deg=width_deg
      ) %>%
      sf::st_sfc() %>%
      sf::st_set_crs(3857) %>%
      sf::st_transform(crs=4326)
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


#' Archive active fire files were downloaded one year by one year
#' which creates memory issues. We split them in more digestible ones.
#' Should only be run once, manually on downloaded files.
#'
#' @return
#' @export
#'
#' @examples
frp.split_yearly_activefire_by_date <- function(f){
  
  d <- data.table::fread(f)
  dates <- unique(d$acq_date)
  
  write_date <- function(date, d, f){
    f_date <- file.path(dirname(f),
                        gsub("[0-9]{4}", strftime(date, "%Y%j"),
                             gsub("csv", "txt", basename(f))))
    write.csv(d[d$acq_date==date], f_date, row.names=FALSE)
  }
  pblapply(dates, write_date, d=d, f=f)
}