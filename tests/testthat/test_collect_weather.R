# tests for weather collection ----
## test parameters ----
temp_dir <- tempdir()

location_id <- "haikou_chn.9_1_cn"
date_from <- lubridate::ymd("2025-01-01")
date_to <- lubridate::ymd("2025-01-03")
weather_vars <- c(
  'air_temp_min','air_temp_max', 'atmos_pres',
  'wd', 'ws', 'precip', 'dewpoint_temp', 'pbl_min', 'pbl_max'
)
n_per_location <- 4
update_era5 <- TRUE
years_force_refresh <- lubridate::year(lubridate::today())

test_that("location_dates has the correct data", {
  location_dates <- rcrea::locations(id = location_id, with_source = FALSE) %>%
    distinct(location_id = id, country, geometry) %>%
    mutate(date_from = date_from, date_to = date_to)
  
  expect_true("geometry" %in% colnames(location_dates))
  expect_true(!is.null(location_dates$geometry))
  expect_true("sfc_POINT" %in% class(location_dates$geometry))
})

test_that("weather vars conversion is working properly", {
  expect_true(era5.rename_global_to_era5("atmos_pres") == "sp")
  expect_true(era5.rename_global_to_era5("precip") == "total_precip")
  expect_true(era5.rename_global_to_era5("air_temp_min") == "temp_min")
  
  expect_true(
    length(era5.rename_global_to_era5("abc", only_keep_existing_vars = TRUE)) == 0
  )
  expect_true(
    era5.rename_global_to_era5("abc", only_keep_existing_vars = FALSE) == "abc"
  )
})

test_that("era5.download_nc is working properly")

weather_era5 <- era5.collect_weather(
  location_dates = location_dates,
  weather_vars = weather_vars,
  update = update_era5
)

weather_noaa <- noaa.collect_weather(
  location_dates = location_dates,
  weather_vars = weather_vars,
  n_per_location = n_per_location,
  years_force_refresh = years_force_refresh
)



weather_vars <- unique(c(weather_vars, "sunshine"))
weather_sources <- unique(c(weather_sources, "sirad"))
weather_sirad <- sirad.collect_weather(location_dates = location_dates)



