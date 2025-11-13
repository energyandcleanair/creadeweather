library(testthat)
library(lubridate)
library(dplyr)
library(tibble)

# Load the package - use devtools::load_all or library
if (!("creadeweather" %in% loadedNamespaces())) {
  devtools::load_all("../../")
}

test_that("prep_data enriches, fills, lags, and filters correctly", {
  set.seed(123)

  # Build synthetic meas_weather for one location over recent dates
  dates <- seq(as.Date("2021-01-01"), by = "day", length.out = 10)
  tbl <- data.frame(
    date = as.Date(dates),
    timezone = "UTC",
    value = seq_len(10),
    wd = c(0, 45, 90, 135, 180, 225, 270, 315, 40, 85),
    ws = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    precip = c(NA, rep(0.1, 9)),
    atmos_pres = c(1010, 1011, NA, 1013, 1014, 1015, 1016, 1017, 1018, 1019),
    air_temp_min = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    air_temp_max = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
    dewpoint_temp = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5),
    pbl_min = seq(100, by = 10, length.out = 10),
    pbl_max = seq(200, by = 10, length.out = 10)
  )

  # Wrap into the input structure expected by prep_data()
  data <- tibble(
    location_id = 1L,
    meas_weather = list(tbl)
  )

  weather_vars <- c('wd','ws','precip','atmos_pres','air_temp_min','air_temp_max',
                    'dewpoint_temp','pbl_min','pbl_max')

  out <- prep_data(
    data = data,
    weather_vars = weather_vars,
    lag = 1
  )

  # After prep, meas_weather should be non-empty and have expected enrichments
  expect_equal(nrow(out), 1)
  mw <- out$meas_weather[[1]]
  expect_gt(nrow(mw), 0)

  # Enrichment: wd_factor and time vars
  expect_true("wd_factor" %in% names(mw))
  expect_true(all(c("yday", "date_unix") %in% names(mw)))

  # Lagging: columns for lagged weather_vars exist (suffix _lag1) and are shifted
  expected_lag_cols <- paste0(weather_vars, "_lag1")
  # utils.add_lag applies to provided weather_vars; not all may be present if filtered
  expect_true(all(expected_lag_cols %in% names(mw)))

  # Check shifting for one exemplar variable
  if (nrow(mw) >= 2) {
    expect_equal(mw$wd_lag1[2], mw$wd[1])
    expect_equal(mw$ws_lag1[2], mw$ws[1])
  }

  # Filling: precip NA becomes 0; atmos_pres NA interpolated
  expect_false(any(is.na(mw$precip)))
  # There was one NA in atmos_pres within a small gap; should be filled by interpolation
  expect_false(any(is.na(mw$atmos_pres)))

  # Filtering: only rows with complete original weather_vars kept
  # Ensure original weather vars are complete after fills/interpolation
  expect_true(all(stats::complete.cases(mw[weather_vars])))
})


