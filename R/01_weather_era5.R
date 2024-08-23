era5.corr_weather_vars <- function() {
  # dict[era5_var] = weather_variable
  list(
    "temp_min" = "air_temp_min",
    "temp_max" = "air_temp_max",
    "sp" = "atmos_pres",
    "wd" = "wd",
    "ws" = "ws",
    "total_precip" = "precip",
    # "humid" = "humidity", # Not valid
    "dewpoint_temp" = "dewpoint_temp",
    "pbl_min" = "pbl_min",
    "pbl_max" = "pbl_max"
  )
}


era5.rename_global_to_era5 <- function(global_var,
                                       only_keep_existing_vars = F) {
  corr <- era5.corr_weather_vars()
  corr_reverse <- as.list(setNames(names(corr), corr))
  x <- recode(global_var, !!!corr_reverse)
  if (only_keep_existing_vars) x <- x[x %in% names(corr)]
  x
}


era5.rename_era5_to_global <- function(era5_var) {
  corr_vars <- era5.corr_weather_vars()
  recode(era5_var, !!!corr_vars)
}


era5.rename_era5_to_global_df <- function(data) {
  data %>%
    rename_all(era5.rename_era5_to_global)
}


#' Adding ERA5 weather data to the weather data frame
#'
#' @param dates: a list of dates for which we want weather data
#' @param locations: a dataframe with location_id and geometry columns
#' @param vars: the list of variables we are interested in
#'
#' @return a data frame with location_id, geometry, date, temp_min, temp_max ...
#' @export
#'
#' @examples
era5.collect_weather <- function(location_dates,
                                 weather_vars,
                                 update = T) {
  dates <- location_dates %>%
    as.data.frame() %>%
    group_by(location_id) %>%
    dplyr::mutate(date = list(seq.Date(date(date_from), date(date_to), by = "1 day"))) %>%
    select(date) %>%
    tidyr::unnest(date) %>%
    pull(date) %>%
    unique()

  if(update){
    print("=== Refreshing ERA5 files ===")
    era5.refresh_files(dates)
    print("=== Done ===")  
  }
  
  locations_sf <- st_as_sf(location_dates %>%
    ungroup() %>%
    dplyr::select(location_id, geometry) %>%
    dplyr::distinct(location_id, .keep_all = T))


  dir_era5 <- era5.folder_era5()
  print(glue("Found {length(list.files(dir_era5, '\\\\.tif'))} tif files in ERA5 folder")) 
  weather_vars_era5 <- era5.rename_global_to_era5(weather_vars,
    only_keep_existing_vars = T
  )

  print(glue("=== Extracting ERA5 for {length(dates)} dates at {nrow(locations_sf)} locations ==="))

  coords <- terra::vect(locations_sf)
  era5_data <- pbmcapply::pbmclapply(dates, function(date) {
    tryCatch(
      {
        tif_file <- era5.date_to_filepaths(date, dir_era5)
        if (!file.exists(tif_file)) {
          return(NULL)
        }
        tif <- terra::rast(tif_file)
        # Only keep the variables we are interested in
        tif <- terra::subset(tif, names(tif) %in% weather_vars_era5)
        tb <- tibble("date" = date)
        extracted_values <- terra::extract(tif, coords)
        
        # Determine the intersection of the specified variables and weather_vars_era5
        temp_vars <- intersect(c("temp_min", "temp_max"), weather_vars_era5)
        pressure_vars <- intersect(c("sp"), weather_vars_era5)
        precip_vars <- intersect(c("total_precip"), weather_vars_era5)
        
        dplyr::bind_cols(location_id = coords$location_id,
                         tb,
                         as.data.frame(extracted_values) %>%
                           dplyr::select(-c(ID))) %>%
                           # K to C for temp_min and temp_max
          dplyr::mutate_at(all_of(temp_vars), ~ . - 273.15) %>%
          # Pa to millibar for atmos_pres
          dplyr::mutate_at(all_of(pressure_vars), ~ . / 1e2) %>%
          # m to mm for precip
          dplyr::mutate_at(all_of(precip_vars), ~ . * 1e3)
      },
      error = function(error) {
        return(NULL)
      }
    )
  }, mc.cores = parallel::detectCores() - 1) %>%
    bind_rows()


  weather <- era5_data %>%
    era5.rename_era5_to_global_df() %>%
    select(c(
      "location_id", "date",
      intersect(names(.), weather_vars)
    )) %>%
    group_by(location_id) %>%
    tidyr::nest() %>%
    rename(weather = data)
  print("=== Done ===")

  return(weather)
}


era5.date_to_filepath <- function(date, extension = "tiff") {
  fname <- paste0("era5_", strftime(as.Date(date), "%Y-%m-%d"), ".", extension)
  file.path(era5.folder_era5(), fname)
}

era5.folder_era5 <- function() {
  dir_era5 <- utils.get_env("DIR_ERA5")
  if (dir_era5 == "") {
    stop("Missing DIR_ERA5 folder")
  }
  return(dir_era5)
}

extract_date <- function(fname) {
  return(strsplit(strsplit(fname, split = "[_]")[[1]][2], split = "[.]")[[1]][1])
}

era5.processed_dates <- function() {
  dir_era5 <- era5.folder_era5()
  ds <- list.files(
    path = dir_era5,
    pattern = "*.tif",
    full.names = F,
    recursive = TRUE
  )
  dates <- unique(lubridate::ymd(lapply(as.list(ds), extract_date)))
  # dates <- unique(lapply(as.list(ds), extract_date))
  date <- dates[!is.na(dates)]
  return(c(date))
}

era5.processable_dates <- function(dates) {
  # Returns list of dates (in dates argument) that are available on CDS
  # All dates are available on CDS (few exceptions, but no easy way to get in advance)
  return(dates[dates < lubridate::today() - 3])
}


#' Build global daily tif files with temp_min, temp_max, temp_mean, RH_min, RH_max etc.
#' And result files are saved in dir_era5
#'
#' @param date
#' @param cache_rs
#'
#' @return
#' @export
#'
#' @examples
era5.process_date <- function(date, force_redownload_nc = F, remove_nc = T, min_layers=24, time_out=NULL) {
  tryCatch(
    {
      era5_long_vars <- c(
        "boundary_layer_height",
        "10m_u_component_of_wind",
        "10m_v_component_of_wind",
        "2m_temperature",
        "2m_dewpoint_temperature",
        "surface_pressure",
        "total_precipitation"
      )

      fname <- paste0("era5_", date, ".nc")
      fpath <- file.path(era5.folder_era5(), fname)
      
      # Downlad nc file if need be
      era5.download_nc(force=force_redownload_nc,
                       date=date,
                       era5_long_vars=era5_long_vars,
                       file_path=fpath,
                       min_layers=min_layers,
                       time_out=time_out)
      
      layers_dict <- list()

      
      calc_raster <- function(fpath, var, fun) {
        brick <-raster::brick(fpath, var = var)
        if(raster::nlayers(brick) < min_layers){
          stop("Not enough layers")
        }
        raster::calc(brick, fun = fun, na.rm = T)
      }

      layers_dict["dewpoint_temp"] <- calc_raster(fpath, "d2m", mean)
      layers_dict["pbl"] <- calc_raster(fpath, "blh", mean)
      layers_dict["pbl_min"] <- calc_raster(fpath, "blh", min)
      layers_dict["pbl_max"] <- calc_raster(fpath, "blh", max)
      layers_dict["temp"] <- calc_raster(fpath, "t2m", mean)
      layers_dict["temp_max"] <- calc_raster(fpath, "t2m", max)
      layers_dict["temp_min"] <- calc_raster(fpath, "t2m", min)

      # Wind direction and wind speed calculation
      # We do it hour by hour and then average wd and ws
      u10_lyrs <- paste0("u10_", seq(1, 24))
      v10_lyrs <- paste0("v10_", seq(1, 24))
  
      uvs <- terra::rast(file.path(era5.folder_era5(), fname), lyrs=c(u10_lyrs, v10_lyrs))
      
      uv2wdws <- function(uv) {
        degrees <- function(radians) 180 * radians / pi
        mathdegs <- degrees(atan2(uv[[2]][], uv[[1]][]))
        wdcalc <- case_when(mathdegs > 0 ~ mathdegs, T ~mathdegs + 360)
        wd <- case_when(wdcalc < 270 ~ 270 - wdcalc, T ~270 - wdcalc + 360)
        ws <- sqrt(uv[[1]][]^2 + uv[[2]][]^2)
        
        uv[] <- cbind(wd, ws)
        names(uv) <- c('wd' ,'ws')
        return(uv)
      }

      uvs2wdws <- function(uvs){
        
        # For each hour
        wdwss <- pbapply::pblapply(seq(1, 24), function(i){
          uv2wdws(uvs[[c(paste0("u10_", i), paste0("v10_", i))]])
        })
        
        # Average per day
        wdwss = terra::rast(wdwss)
        ws <- terra::app(wdwss['^ws$'], mean)
        
        # For wind direction, we sum u and v across all hours and take the resulting direction
        wd <- uv2wdws(terra::rast(list(terra::app(uvs['u10'], sum), terra::app(uvs['v10'], sum))))['wd']
        
        return(list(wd=wd, ws=ws))
      }
      
      wdws <- uvs2wdws(uvs)

      layers_dict["wd"] <- raster::raster(wdws$wd)
      layers_dict["ws"] <- raster::raster(wdws$ws)

      # # Humidity calculation: https://confluence.ecmwf.int/pages/viewpage.action?pageId=171411214
      # Rdry <- 287.0597 # Constant for dry air J kg−1 K−1
      # Rvap <- 461.5250 # Constant for water vapor J kg−1 K−1
      # # a1, a3, a4, and T0:
      # # Parameters for Teten's formula according to Buck (1981)
      # # Buck, A. L. (1981). New equations for computing vapor pressure and
      # # enhancement factor. J. Appl. Meteorol., 20, 1527–1532
      # a1 <- 611.21 # units: Pa
      # a3 <- 17.502 # units: K
      # a4 <- 32.19 # units: K
      # T0 <- 273.16 # units: K
      # 
      # # Written by Stefanos. Hubert: doesn't seem to give decent values
      # # let's use dewpoint temp instead
      # humidity_fun <- function(t2msp) {
      #   E <- exp(17.502 * ((t2msp[1] - 273.16) / (t2msp[1] - 32.19)))
      #   huss <- (0.6219808 * E) / (t2msp[2] - ((1 - Rdry / Rvap) * E))
      #   # ps-((1-Rdry/Rvap)*E)
      #   return(huss)
      # }
      # huss <- raster::calc(t2msp, humidity_fun)
      # layers_dict["humid"] <- huss

      sp <- calc_raster(fpath, "sp", mean)
      tp <- calc_raster(fpath, "tp", sum)
      t2m <- layers_dict["temp"]
      t2msp <- raster::stack(c(t2m, sp))

      layers_dict["sp"] <- sp
      layers_dict["total_precip"] <- tp

      output_path <- file.path(era5.folder_era5(), paste0(strsplit(fname, split = "[.]")[[1]][1], ".tiff"))

      tif <- raster::stack(layers_dict)
      names(tif) <- names(layers_dict)

      terra_ras <- terra::rast(tif)
      t <- terra::writeRaster(terra_ras, output_path, overwrite = TRUE)
      if(remove_nc) file.remove(file.path(era5.folder_era5(), fname))
    },
    error = function(error) {
      # QUICKFIX try print as well
      print(paste("Failed for date", date, error))
      warning(paste("Failed for date", date, error))
      return(NULL)
    }
  )
}



#' Generate the missing files for indicated dates
#'
#' @param dates
#'
#' @return
#' @export
#'
#' @examples
era5.refresh_files <- function(dates) {
  processed <- era5.processed_dates()
  processable <- era5.processable_dates(dates)
  to_process <- processable[!processable %in% processed]

  cache_rs <- NULL
  for (i in seq_along(to_process)) {
    d <- to_process[i]
    print(paste("Processing", d))
    era5.process_date(d)
  }
}

era5.date_to_filepaths <- function(date, dir_era5) {
  # returns the path to the daily file for date `date`
  return(file.path(dir_era5, paste0("era5_", date, ".tiff")))
}

era5.reprocess_files <- function(date_from='2015-01-01', date_to=lubridate::today(), force = F){
  
  dates <- seq.Date(as.Date(date_from), as.Date(date_to), by = "1 day")
  dir_era5 <- era5.folder_era5()
  
  n_layers <- pbapply::pbsapply(dates, function(date){
    tif_file <- era5.date_to_filepaths(date, dir_era5)
    if (!file.exists(tif_file)) {
      return(0)
    }
    tif <- terra::rast(tif_file)
    return(length(names(tif)))
  })

  dates_to_process <- dates[force | n_layers < 11]
  pbapply::pblapply(dates_to_process, era5.process_date, remove_nc = T, time_out=3600)
  
}

era5.download_nc <- function(force,
                             date,
                             file_path,
                             era5_long_vars,
                             min_layers=NULL,
                             time_out=NULL)
{
  
  do_download <- force | !file.exists(file_path)
  
  if(!is.null(min_layers) & file.exists(file_path)){
    n_layers <- suppressWarnings(raster::nlayers(raster::brick(file_path)))
    if(n_layers < min_layers){
      message(glue("Redownloading {file_path} as it seems incomplete"))
      do_download <- TRUE
    }
  }
  
  if(do_download){
    year <- strsplit(as.character(date), split = "[-]")[[1]][1]
    month <- strsplit(as.character(date), split = "[-]")[[1]][2]
    day <- strsplit(as.character(date), split = "[-]")[[1]][3]
    request <- list(
      dataset_short_name = "reanalysis-era5-single-levels",
      product_type = "reanalysis",
      format = "netcdf",
      variable = era5_long_vars,
      year = year,
      month = month,
      day = day,
      time = c(
        "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00",
        "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
        "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"
      ),
      # area is specified as N, W, S, E
      area = c(90, -180, -90, 180),
      target = basename(file_path)
    )
    
    if (!"ecmwfr" %in% keyring::keyring_list()$keyring) {
      keyring::keyring_create(keyring = "ecmwfr", password = utils.get_env("KEYRING_PASSWORD"))
    }
    
    try(keyring::keyring_unlock(keyring = "ecmwfr", password = utils.get_env("KEYRING_PASSWORD")))
    
    ecmwfr::wf_set_key(
      user = utils.get_env("CDS_UID", error_if_not_found = T),
      key = utils.get_env("CDS_API_KEY", error_if_not_found = T),
      service = "cds"
    )
    
    # Timeout: short if recent date, long otherwise
    # as it is likely to be unavailable
    if(is.null(time_out)){
      if(lubridate::today() - as.Date(date) < 10){
        time_out <- 60
      } else {
        time_out <- 300
      }  
    }
 
    ecmwfr::wf_request(
      user = utils.get_env("CDS_UID"),
      request = request,
      transfer = TRUE,
      path = dirname(file_path),
      time_out = time_out,
      verbose = TRUE
    )
  }
}
