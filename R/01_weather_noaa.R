noaa.corr_weather_vars <- function() {
  # dict[noaa_var] = weather_variable
  list(
    "air_temp_min" = "air_temp_min",
    "air_temp_max" = "air_temp_max",
    "atmos_pres" = "atmos_pres",
    "wd" = "wd",
    "ws" = "ws",
    "precip" = "precip",
    "RH_max" = "humidity"
  )
}

#' Rename dataframe from NOAA to GLOBAL names
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
noaa.rename_noaa_to_global <- function(noaa_var) {
  corr_vars <- noaa.corr_weather_vars()
  recode(noaa_var, !!!corr_vars)
}

noaa.rename_noaa_to_global_df <- function(data) {
  data %>% rename_all(noaa.rename_noaa_to_global)
}

noaa.collect_weather <- function(
  location_dates,
  weather_vars,
  n_per_location,
  years_force_refresh
) {
  # Find weather stations nearby
  locations_w_stations <- noaa.add_close_stations(
    location_dates = location_dates,
    n_per_location = n_per_location
  )

  # Get weather at these stations
  locations_weather <- noaa.add_weather(
    locations_w_stations = locations_w_stations,
    weather_vars = weather_vars,
    years_force_refresh = years_force_refresh
  )
  
  if (is.null(locations_weather)) return(NULL)
  
  # Ensure columns are preserved
  result <- location_dates %>% left_join(locations_weather)

  result
}


noaa.add_close_stations <- function(location_dates, n_per_location) {
  suppressMessages(location_dates %>%
    rowwise() %>%
    mutate(noaa_station = list(
      worldmet::getMeta(
        lat = sf::st_coordinates(geometry)[, 2],
        lon = sf::st_coordinates(geometry)[, 1],
        n = n_per_location,
        plot = FALSE
      ) %>% # radius is in km
        dplyr::filter(end >= "2020-01-01", dist < 100) %>%
        dplyr::arrange(desc(end), dist) %>%
        dplyr::slice(1:n_per_location)
    ))
  )
}



noaa.available_years <- function(code) {
  
  d <- suppressMessages(worldmet::getMetaLive()) %>%
    filter(paste0(USAF, "-", WBAN) == !!code) %>%
    reframe(years = seq(year(BEGIN), year(END)))
  
  d$years
}


noaa.valid_years_cached <- function(code, years, cache_folder) {
  files <- list.files(
    path = cache_folder, 
    pattern = paste0(code, "_", years, ".rds", collapse = "|"), 
    full.names = TRUE
  )
  if (length(files) == 0) {
    return(c())
  }

  years <- as.numeric(stringr::str_extract(files, "(?<=_)\\d{4}(?=\\.rds)"))
  # Only keep files who have been updated after the end of the year
  # But also keep those who have been updated today

  is_valid <- file.info(files)$mtime >= pmin(
    as.POSIXct(paste0(years + 1, "-01-01")),
    lubridate::today()
  )
  years[is_valid]
}


noaa.get_noaa_at_code <- function(
  code, 
  year_from, 
  year_to, 
  years_force_refresh = NULL, 
  cache_folder
) {
  # Get NOAA data, first trying to use cached files for complete years
  tryCatch(
    {
      # Reading cache files
      years <- seq(year_from, year_to)
      years_available <- noaa.available_years(code)
      years <- intersect(years, years_available)
      years_try_cache <- setdiff(years, years_force_refresh)
      years_cached <- noaa.valid_years_cached(code, years_try_cache, cache_folder)
      files_cached <- list.files(
        path = cache_folder, 
        pattern = paste0(code, "_", years_cached, ".rds", collapse = "|"), 
        full.names = TRUE
      )
      readFile <- function(path) {
        if (file.exists(path)) readRDS(path) else NULL
      }

      data_cached <- files_cached %>% purrr::map_dfr(readFile) %>% bind_rows()

      years_to_download <- unique(c(setdiff(years, years_cached), years_force_refresh))

      download_data <- function(year, code, cache_folder) {
        tryCatch(
          {
            worldmet::importNOAA(
              code = code,
              year = year,
              quiet = TRUE,
              path = cache_folder
            )
          }, error = function(cond) {
            warning(
              paste("Failed to get new data for years", years_to_download, ". Using cache instead")
            )
            files <- list.files(
              path = cache_folder, 
              pattern = paste0(code, "_", year, ".rds", collapse = "|"), 
              full.names = TRUE
            )
            tryCatch(
              {
                files %>% purrr::map_dfr(readFile) %>% bind_rows()
              }, error = function(c) {
                print("DEBUG: ERROR IN importNOAA")
                NULL
              }
            )
          }
        )
      }

      # Downloading fresh data
      if (length(years_to_download) > 0) {
        data_downloaded <- do.call(
          "bind_rows",
          lapply(years_to_download, download_data, code = code, cache_folder = cache_folder)
        )
      } else {
        data_downloaded <- NULL
      }

      # Binding and returning
      result <- bind_rows(data_cached, data_downloaded)
      if (nrow(result) == 0) {
        return(NULL)
      }

      # Aggregate per day
      # Note: Certain dates have no data at all and hence raise warnings
      suppressWarnings(result <- result %>%
        dplyr::group_by(date = lubridate::date(date)) %>%
        dplyr::summarize(
          air_temp_min = min(air_temp, na.rm = TRUE),
          air_temp_max = max(air_temp, na.rm = TRUE),
          air_temp = mean(air_temp, na.rm = TRUE),
          atmos_pres = mean(atmos_pres, na.rm = TRUE),
          wd = weighted.mean(wd, w = ws, na.rm = TRUE),
          ws_max = max(ws, na.rm = TRUE),
          ws = mean(ws, na.rm = TRUE),
          ceil_hgt = mean(ceil_hgt, na.rm = TRUE),
          visibility = mean(visibility, na.rm = TRUE),
          precip = mean(precip, na.rm = TRUE),
          RH = mean(RH / 100, na.rm = TRUE),
          RH_min = min(RH, na.rm = TRUE),
          RH_max = max(RH, na.rm = TRUE)
        ) %>%
        # Replace infinite with NA
        mutate_at(vars(-date), ~ ifelse(is.infinite(.x), NA, .x)))

      result
    }, error = function(err) {
      message(err)
      warning(err)
      NULL
    }
  )
}


noaa.get_folder <- function() {
  dir_noaa <- Sys.getenv("DIR_NOAA_ISD")
  dir.create(dir_noaa, showWarnings = FALSE, recursive = TRUE)
  if (!dir.exists(dir_noaa)) {
    stop("Failed to read/create DIR_NOAA_ISD")
  }
  dir_noaa
}


noaa.add_weather <- function(
  locations_w_stations,
  weather_vars,
  years_force_refresh = year(today())
) {

  print("Adding weather from NOAA")
  cache_folder <- noaa.get_folder()

  weather <- locations_w_stations %>%
    dplyr::ungroup() %>%
    as.data.frame() %>%
    tidyr::unnest(cols = (noaa_station)) %>%
    dplyr::distinct(location_id, usaf, wban, dist, date_from, date_to)

  weather$code <- paste(weather$usaf, weather$wban, sep = "-")
  # print(paste("Codes:", paste(unique(stations_weather$code)), collapse=","))

  weather$weather_noaa <- tryCatch(
    {
      pbapply::pbmapply(noaa.get_noaa_at_code,
        code = weather$code,
        year_from = lubridate::year(weather$date_from),
        year_to = lubridate::year(weather$date_to),
        years_force_refresh = list(years_force_refresh),
        cache_folder = cache_folder,
        SIMPLIFY = FALSE
      )
    }, error = function(err) {
      warning(err)
      NA
    }
  )


  to_date <- function(d) {
    tryCatch(
      {
        lubridate::date(d)
      }, error = function(err) {
        print(d)
        NA
      }
    )
  }

  if (nrow(weather %>% rowwise() %>% filter(!is.null(weather_noaa))) == 0) {
    warning("Failed to find weather data with NOAA. Returning NULL")
    return(NULL)
  }

  # Note: Certain dates have no data at all and hence raise warnings
  locations_weather <- weather %>%
    unnest(weather_noaa) %>%
    group_by(location_id) %>%
    mutate(date = to_date(date)) %>%
    filter(!is.na(date)) %>%
    filter(date >= unique(date_from)) %>%
    filter(date <= unique(date_to)) %>%
    group_by(location_id, date) %>%
    # weighted mean by 1/pmax(1,dist) 
    summarize_at(
      vars(
        air_temp_min, air_temp_max, air_temp, atmos_pres, wd, ws_max, 
        ws, ceil_hgt, visibility, precip, RH
      ),
      ~ weighted.mean(.x, w = 1 / pmax(1, dist), na.rm = TRUE)
    ) %>%
    noaa.rename_noaa_to_global_df() %>%
    select(c("location_id", "date", intersect(names(.), weather_vars))) %>%
    group_by(location_id) %>%
    tidyr::nest() %>%
    rename(weather = data)

  return(locations_weather)
}