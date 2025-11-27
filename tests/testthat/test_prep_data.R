# =============================================================================
# Unit tests for prep_data module (R/02_prepare_data.R)
#
# Tests the data preparation pipeline for model training:
# - clean_data(): Outlier removal, infinite value handling
# - fill_data(): Missing value strategies (zero-fill, interpolation)
# - enrich_data(): Feature engineering (wind sectors, lagged variables)
# - filter_data(): Completeness filtering
# - prep_data(): Full pipeline integration
# =============================================================================


# =============================================================================
# Test fixtures
# =============================================================================

#' Create a base meas_weather tibble for testing
#'
#' @param n_days Number of days of data to generate
#' @param start_date Starting date for the series
#' @param include_na Whether to include NA values for testing fill behavior
#' @param include_outlier Whether to include an extreme outlier value
create_meas_weather <- function(
    n_days = 10,
    start_date = as.Date("2021-01-01"),
    include_na = FALSE,
    include_outlier = FALSE
) {
  dates <- seq(start_date, by = "day", length.out = n_days)

  # Base pollution values
  values <- seq_len(n_days) * 10

  # Add outlier if requested (extreme value at position 5)
  if (include_outlier && n_days >= 5) {
    values[5] <- 10000
  }

  # Wind direction covering all 8 sectors
  wd <- rep(c(0, 45, 90, 135, 180, 225, 270, 315), length.out = n_days)

  # Precipitation and atmos_pres with optional NAs
  precip <- rep(0.1, n_days)
  atmos_pres <- seq(1010, by = 1, length.out = n_days)

  if (include_na) {
    precip[1] <- NA
    atmos_pres[3] <- NA
  }

  data.frame(
    date = dates,
    timezone = "UTC",
    value = values,
    wd = wd,
    ws = seq_len(n_days),
    precip = precip,
    atmos_pres = atmos_pres,
    air_temp_min = seq(0, by = 1, length.out = n_days),
    air_temp_max = seq(5, by = 1, length.out = n_days),
    dewpoint_temp = seq(0, by = 0.5, length.out = n_days),
    pbl_min = seq(100, by = 10, length.out = n_days),
    pbl_max = seq(200, by = 10, length.out = n_days)
  )
}

#' Wrap meas_weather into the structure expected by prep_data()
create_prep_data_input <- function(meas_weather, location_id = 1L) {
  tibble::tibble(
    location_id = location_id,
    meas_weather = list(meas_weather)
  )
}

#' Standard weather variables used across tests
WEATHER_VARS <- c(
  "wd", "ws", "precip", "atmos_pres",
  "air_temp_min", "air_temp_max", "dewpoint_temp",
  "pbl_min", "pbl_max"
)


# =============================================================================
# Tests for clean_data()
# =============================================================================

test_that("clean_data removes infinite values", {
  mw <- create_meas_weather()
  mw$ws[3] <- Inf
  mw$atmos_pres[5] <- -Inf

  result <- creadeweather:::clean_data(mw)

  expect_false(any(is.infinite(result$ws), na.rm = TRUE))
  expect_false(any(is.infinite(result$atmos_pres), na.rm = TRUE))
})

test_that("clean_data removes outliers using MAD filter", {
  mw <- create_meas_weather(include_outlier = TRUE)
  n_before <- nrow(mw)

  result <- suppressMessages(creadeweather:::clean_data(mw))

  # Outlier row should be removed
expect_lt(nrow(result), n_before)
  expect_false(10000 %in% result$value)
})

test_that("clean_data converts NaN to NA", {
  mw <- create_meas_weather()
  mw$ws[2] <- NaN

  result <- creadeweather:::clean_data(mw)

  expect_false(any(is.nan(result$ws)))
})

test_that("clean_data removes rows with NA dates", {
  mw <- create_meas_weather()
  mw$date[4] <- NA

  result <- creadeweather:::clean_data(mw)

  expect_false(any(is.na(result$date)))
  expect_lt(nrow(result), nrow(mw))
})


# =============================================================================
# Tests for fill_data()
# =============================================================================

test_that("fill_data replaces precip NA with 0", {
  mw <- create_meas_weather(include_na = TRUE)
  expect_true(any(is.na(mw$precip)))

  result <- creadeweather:::fill_data(mw)

  expect_false(any(is.na(result$precip)))
  expect_equal(result$precip[1], 0)
})

test_that("fill_data interpolates atmos_pres gaps", {
  mw <- create_meas_weather(include_na = TRUE)
  expect_true(any(is.na(mw$atmos_pres)))

  result <- creadeweather:::fill_data(mw)

  expect_false(any(is.na(result$atmos_pres)))
  # Interpolated value should be between neighbors
  expect_equal(result$atmos_pres[3], mean(c(mw$atmos_pres[2], mw$atmos_pres[4])))
})

test_that("fill_data replaces all-NA column with zeros", {
  mw <- create_meas_weather()
  mw$pbl_min <- NA

  result <- creadeweather:::fill_data(mw)

  expect_false(any(is.na(result$pbl_min)))
  expect_true(all(result$pbl_min == 0))
})

test_that("fill_data handles fire variables", {
  mw <- create_meas_weather()
  mw$fire_frp <- c(NA, 1, 2, NA, 3, NA, 4, 5, NA, 6)

  result <- creadeweather:::fill_data(mw)

  # Fire NAs should become 0
  expect_false(any(is.na(result$fire_frp)))
  expect_equal(result$fire_frp[1], 0)
  expect_equal(result$fire_frp[4], 0)
})


# =============================================================================
# Tests for enrich_data()
# =============================================================================

test_that("enrich_data adds wind direction factor with 8 sectors", {
  mw <- create_meas_weather()

  result <- creadeweather:::enrich_data(mw, lag = 0, weather_vars = WEATHER_VARS)

  expect_true("wd_factor" %in% names(result))
  expect_s3_class(result$wd_factor, "factor")
  # 8 sectors: 0-7
  expect_true(all(as.integer(as.character(result$wd_factor)) %in% 0:7))
})

test_that("enrich_data creates correct wind sectors", {
  mw <- data.frame(
    date = as.Date("2021-01-01") + 0:7,
    wd = c(0, 45, 90, 135, 180, 225, 270, 315)
  )

  result <- creadeweather:::enrich_data(mw, lag = 0, weather_vars = c())

  expected_sectors <- as.character(0:7)
  expect_equal(as.character(result$wd_factor), expected_sectors)
})

test_that("enrich_data creates lagged variables with correct suffix", {
  mw <- create_meas_weather()

  result <- creadeweather:::enrich_data(mw, lag = 2, weather_vars = WEATHER_VARS)

  # Check lag1 columns exist
  lag1_cols <- paste0(WEATHER_VARS, "_lag1")
  expect_true(all(lag1_cols %in% names(result)))

  # Check lag2 columns exist
  lag2_cols <- paste0(WEATHER_VARS, "_lag2")
  expect_true(all(lag2_cols %in% names(result)))
})

test_that("enrich_data lagged values are correctly shifted", {
  mw <- create_meas_weather()

  result <- creadeweather:::enrich_data(mw, lag = 1, weather_vars = WEATHER_VARS)

  # Row 2's lag1 should equal row 1's original value
  expect_equal(result$ws_lag1[2], result$ws[1])
  expect_equal(result$wd_lag1[2], result$wd[1])
  expect_equal(result$precip_lag1[2], result$precip[1])
})

test_that("enrich_data with lag=0 creates no lag columns", {
  mw <- create_meas_weather()

  result <- creadeweather:::enrich_data(mw, lag = 0, weather_vars = WEATHER_VARS)

  lag_cols <- grep("_lag", names(result), value = TRUE)
  expect_length(lag_cols, 0)
})


# =============================================================================
# Tests for filter_data()
# =============================================================================

test_that("filter_data keeps only complete cases", {
  mw <- create_meas_weather()
  mw$ws[3] <- NA
  mw$precip[7] <- NA

  result <- creadeweather:::filter_data(mw, weather_vars = WEATHER_VARS)

  expect_true(all(complete.cases(result[WEATHER_VARS])))
  expect_lt(nrow(result), nrow(mw))
})

test_that("filter_data returns NA for old data", {
  mw <- create_meas_weather(start_date = as.Date("2019-01-01"))

  result <- suppressWarnings(
    creadeweather:::filter_data(mw, weather_vars = WEATHER_VARS)
  )

  expect_true(is.na(result))
})

test_that("filter_data ignores lagged columns for completeness check", {
  mw <- create_meas_weather()
  # Add a lag column with NA (should not affect filtering)
  mw$ws_lag1 <- c(NA, mw$ws[-nrow(mw)])

  result <- creadeweather:::filter_data(mw, weather_vars = WEATHER_VARS)

  # Should keep all rows since base weather_vars are complete
  expect_equal(nrow(result), nrow(mw))
})


# =============================================================================
# Tests for prep_data() - full pipeline
# =============================================================================

test_that("prep_data returns correct structure", {
  data <- create_prep_data_input(create_meas_weather())

  result <- prep_data(
    data = data,
    weather_vars = WEATHER_VARS,
    time_vars = c(),
    lag = 1
  )

  expect_equal(nrow(result), 1)
  expect_true("meas_weather" %in% names(result))
  expect_true("location_id" %in% names(result))
  expect_s3_class(result$meas_weather[[1]], "data.frame")
})

test_that("prep_data applies full pipeline correctly", {
  mw <- create_meas_weather(include_na = TRUE)
  data <- create_prep_data_input(mw)

  result <- prep_data(
    data = data,
    weather_vars = WEATHER_VARS,
    time_vars = c(),
    lag = 1
  )

  output <- result$meas_weather[[1]]

  # Enrichment applied
  expect_true("wd_factor" %in% names(output))

  # Lagging applied
  expect_true("ws_lag1" %in% names(output))

  # Filling applied (no NAs in precip or atmos_pres)
  expect_false(any(is.na(output$precip)))
  expect_false(any(is.na(output$atmos_pres)))

  # Filtering applied (complete cases)
  expect_true(all(complete.cases(output[WEATHER_VARS])))
})

test_that("prep_data removes locations with no valid data", {
  old_mw <- create_meas_weather(start_date = as.Date("2018-01-01"))
  new_mw <- create_meas_weather(start_date = as.Date("2021-01-01"))

  data <- tibble::tibble(
    location_id = c(1L, 2L),
    meas_weather = list(old_mw, new_mw)
  )

  result <- suppressWarnings(
    prep_data(
      data = data,
      weather_vars = WEATHER_VARS,
      time_vars = c(),
      lag = 0
    )
  )

  # Only location 2 should remain
  expect_equal(nrow(result), 1)
  expect_equal(result$location_id, 2L)
})

test_that("prep_data handles multiple locations", {
  mw1 <- create_meas_weather(n_days = 10)
  mw2 <- create_meas_weather(n_days = 15, start_date = as.Date("2021-06-01"))

  data <- tibble::tibble(
    location_id = c(1L, 2L),
    meas_weather = list(mw1, mw2)
  )

  result <- prep_data(
    data = data,
    weather_vars = WEATHER_VARS,
    time_vars = c(),
    lag = 1
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$location_id, c(1L, 2L))
})

test_that("prep_data handles errors in individual locations gracefully", {
  good_mw <- create_meas_weather()
  bad_mw <- data.frame(date = as.Date("2021-01-01"))  # Missing required columns

  data <- tibble::tibble(
    location_id = c(1L, 2L),
    meas_weather = list(good_mw, bad_mw)
  )

  # Should not error, just skip the bad location
  result <- suppressWarnings(
    prep_data(
      data = data,
      weather_vars = WEATHER_VARS,
      time_vars = c(),
      lag = 0
    )
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$location_id, 1L)
})
