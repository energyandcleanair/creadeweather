# =============================================================================
# Unit tests for deweather_yoy module (R/deweather_yoy.R)
#
# Tests the year-over-year (YoY) deweathering helper functions used by
# CREA's monthly snapshot products to track pollution trends.
#
# - get_excluded_yoy_dates(): Date range calculation
# - extract_yoy_changes_from_result(): YoY metric computation
# - extract_yoy_changes(): Wrapper for deweathered results
# =============================================================================


# =============================================================================
# Tests for get_excluded_yoy_dates()
# =============================================================================

test_that("get_excluded_yoy_dates returns correct date ranges", {
  dates <- get_excluded_yoy_dates("2024-07-01")

  # Should include July 2023 and July 2024
  expect_true(as.Date("2023-07-01") %in% dates)
  expect_true(as.Date("2023-07-31") %in% dates)
  expect_true(as.Date("2024-07-01") %in% dates)
  expect_true(as.Date("2024-07-31") %in% dates)

  # Should have 62 days total (31 + 31)
  expect_equal(length(dates), 62)
})

test_that("get_excluded_yoy_dates handles February correctly", {
  # 2024 is a leap year, 2023 is not
  dates <- get_excluded_yoy_dates("2024-02-01")

  # Feb 2023 has 28 days, Feb 2024 has 29 days
  expect_equal(length(dates), 28 + 29)

  expect_true(as.Date("2023-02-28") %in% dates)
  expect_true(as.Date("2024-02-29") %in% dates)   # Leap year

  # Feb 2023 should have exactly 28 days
  feb_2023_dates <- dates[lubridate::year(dates) == 2023]
  expect_equal(length(feb_2023_dates), 28)
})

test_that("get_excluded_yoy_dates works with different input formats", {
  # Should work with character
  dates1 <- get_excluded_yoy_dates("2024-07-01")

  # Should work with Date object
  dates2 <- get_excluded_yoy_dates(as.Date("2024-07-01"))

  expect_equal(dates1, dates2)
})

test_that("get_excluded_yoy_dates excludes exactly one year apart", {
  dates <- get_excluded_yoy_dates("2024-07-01")

  # All dates should be in July
  months <- lubridate::month(dates)
  expect_true(all(months == 7))

  # Years should be 2023 and 2024 only
  years <- unique(lubridate::year(dates))
  expect_equal(sort(years), c(2023, 2024))
})


# =============================================================================
# Tests for extract_yoy_changes_from_result()
# =============================================================================

#' Create synthetic result data for testing YoY extraction
create_yoy_test_result <- function(
    month = "2024-07-01",
    obs_prev = 50,
    pred_prev = 45,
    obs_current = 55,
    pred_current = 48,
    n_days = 5
) {
  month_date <- as.Date(month)
  month_prev <- month_date - lubridate::years(1)

  # Create dates for both months
  dates_prev <- seq(month_prev, by = "day", length.out = n_days)
  dates_current <- seq(month_date, by = "day", length.out = n_days)

  # Create result dataframe
  tibble::tibble(
    date = c(
      rep(dates_prev, 2),
      rep(dates_current, 2)
    ),
    variable = c(
      rep("observed", n_days), rep("predicted", n_days),
      rep("observed", n_days), rep("predicted", n_days)
    ),
    value = c(
      rep(obs_prev, n_days), rep(pred_prev, n_days),
      rep(obs_current, n_days), rep(pred_current, n_days)
    )
  )
}

test_that("extract_yoy_changes_from_result calculates correct YoY metrics", {
  # Setup: last year obs=50, pred=45; this year obs=55, pred=48
  result <- create_yoy_test_result(
    month = "2024-07-01",
    obs_prev = 50,
    pred_prev = 45,
    obs_current = 55,
    pred_current = 48
  )

  yoy <- extract_yoy_changes_from_result(result, "2024-07-01")

  expect_s3_class(yoy, "data.frame")
  expect_true(all(c("date", "variable", "value") %in% names(yoy)))

  # Helper to get value by variable name
  get_yoy_value <- function(var) yoy$value[yoy$variable == var]

  # yoy_total = 55 - 50 = 5
  expect_equal(get_yoy_value("yoy_total"), 5, tolerance = 0.01)

  # yoy_weather = 48 - 45 = 3
  expect_equal(get_yoy_value("yoy_weather"), 3, tolerance = 0.01)

  # yoy_emission = 5 - 3 = 2
  expect_equal(get_yoy_value("yoy_emission"), 2, tolerance = 0.01)

  # yoy_total_rel = 5 / 50 = 0.1
  expect_equal(get_yoy_value("yoy_total_rel"), 0.1, tolerance = 0.01)

  # yoy_weather_rel = 3 / 50 = 0.06
  expect_equal(get_yoy_value("yoy_weather_rel"), 0.06, tolerance = 0.01)

  # yoy_emission_rel = 2 / 50 = 0.04
  expect_equal(get_yoy_value("yoy_emission_rel"), 0.04, tolerance = 0.01)
})

test_that("extract_yoy_changes_from_result returns 6 variables", {
  result <- create_yoy_test_result()
  yoy <- extract_yoy_changes_from_result(result, "2024-07-01")

  expect_equal(nrow(yoy), 6)
  expected_vars <- c(
    "yoy_total", "yoy_weather", "yoy_emission",
    "yoy_total_rel", "yoy_weather_rel", "yoy_emission_rel"
  )
  expect_true(all(expected_vars %in% yoy$variable))
})

test_that("extract_yoy_changes_from_result handles pollution decrease", {
  # Pollution went down: obs 60->50, pred 55->45
  result <- create_yoy_test_result(
    obs_prev = 60,
    pred_prev = 55,
    obs_current = 50,
    pred_current = 45
  )

  yoy <- extract_yoy_changes_from_result(result, "2024-07-01")
  get_yoy_value <- function(var) yoy$value[yoy$variable == var]

  # yoy_total = 50 - 60 = -10 (decrease)
  expect_equal(get_yoy_value("yoy_total"), -10, tolerance = 0.01)

  # yoy_weather = 45 - 55 = -10
  expect_equal(get_yoy_value("yoy_weather"), -10, tolerance = 0.01)

  # yoy_emission = -10 - (-10) = 0 (all change due to weather)
  expect_equal(get_yoy_value("yoy_emission"), 0, tolerance = 0.01)
})

test_that("extract_yoy_changes_from_result returns NULL for empty result", {
  expect_null(extract_yoy_changes_from_result(NULL, "2024-07-01"))
})

test_that("extract_yoy_changes_from_result returns NULL for missing variables", {
  # Result without "predicted" variable
  result <- tibble::tibble(
    date = as.Date("2024-07-01"),
    variable = "observed",
    value = 50
  )

  yoy <- extract_yoy_changes_from_result(result, "2024-07-01")
  expect_null(yoy)
})

test_that("extract_yoy_changes_from_result returns correct date", {
  result <- create_yoy_test_result(month = "2024-03-01")
  yoy <- extract_yoy_changes_from_result(result, "2024-03-01")

  # All rows should have the target month as date
  expect_true(all(yoy$date == as.Date("2024-03-01")))
})


# =============================================================================
# Tests for extract_yoy_changes()
# =============================================================================

test_that("extract_yoy_changes processes deweathered structure", {
  # Create synthetic deweathered structure
  result <- create_yoy_test_result()

  deweathered <- tibble::tibble(
    location_id = "test_location",
    result = list(result)
  )

  processed <- extract_yoy_changes(deweathered, "2024-07-01", keep_nonyoy_results = FALSE)

  expect_equal(nrow(processed), 1)
  expect_equal(processed$location_id, "test_location")

  # Result should now contain YoY variables
  yoy_result <- processed$result[[1]]
  expect_true(all(grepl("yoy", yoy_result$variable)))
})

test_that("extract_yoy_changes with keep_nonyoy_results=TRUE preserves original", {
  result <- create_yoy_test_result()

  deweathered <- tibble::tibble(
    location_id = "test_location",
    result = list(result)
  )

  processed <- extract_yoy_changes(deweathered, "2024-07-01", keep_nonyoy_results = TRUE)

  combined_result <- processed$result[[1]]

  # Should have both original and YoY variables
  expect_true("observed" %in% combined_result$variable)
  expect_true("predicted" %in% combined_result$variable)
  expect_true("yoy_total" %in% combined_result$variable)
})

test_that("extract_yoy_changes handles multiple locations", {
  result1 <- create_yoy_test_result(obs_prev = 50, obs_current = 60)
  result2 <- create_yoy_test_result(obs_prev = 30, obs_current = 25)

  deweathered <- tibble::tibble(
    location_id = c("location_a", "location_b"),
    result = list(result1, result2)
  )

  processed <- extract_yoy_changes(deweathered, "2024-07-01", keep_nonyoy_results = FALSE)

  expect_equal(nrow(processed), 2)

  # Both should have YoY results
  expect_equal(nrow(processed$result[[1]]), 6)
  expect_equal(nrow(processed$result[[2]]), 6)

  # Helper to get value
  get_val <- function(df, var) df$value[df$variable == var]

  # Values should be different
  expect_equal(get_val(processed$result[[1]], "yoy_total"), 10, tolerance = 0.01)   # 60-50
  expect_equal(get_val(processed$result[[2]], "yoy_total"), -5, tolerance = 0.01)   # 25-30
})

test_that("extract_yoy_changes handles NULL results gracefully", {
  deweathered <- tibble::tibble(
    location_id = c("good_location", "bad_location"),
    result = list(create_yoy_test_result(), NULL)
  )

  processed <- extract_yoy_changes(deweathered, "2024-07-01", keep_nonyoy_results = FALSE)

  # Should still have 2 rows
  expect_equal(nrow(processed), 2)

  # First should have results, second should be NULL
  expect_equal(nrow(processed$result[[1]]), 6)
  expect_null(processed$result[[2]])
})
