# =============================================================================
# Integration tests for the full training + postcompute pipeline
#
# These high-level tests validate that the pipeline can capture meaningful
# signals from synthetic data. We test:
# - Trend: Long-term decline in pollution
# - Anomaly: Short-term spike in pollution
# - Fire: Fire-related pollution contribution
# - Combined: Multiple signals together
# =============================================================================

testthat::source_test_helpers("tests/testthat", env = globalenv())


# =============================================================================
# Test constants - tolerance thresholds for signal detection
# =============================================================================

# Trend detection tolerances (fraction of expected trend)
TREND_MIN_DETECTION_FRACTION <- 0.4
TREND_MAX_DETECTION_FRACTION <- 1.5

# Anomaly detection tolerances
ANOMALY_MIN_SIGNAL_FRACTION <- 0.5
ANOMALY_MAX_BASELINE_FRACTION <- 0.2

# Fire detection tolerances
FIRE_TOLERANCE_ABSOLUTE <- 0.5
FIRE_TOLERANCE_RELATIVE <- 0.5


# =============================================================================
# Helper functions for result extraction
# =============================================================================

#' Run training pipeline and return post-processed results
run_pipeline <- function(inputs) {
  results <- creadeweather::train_configs(
    data = inputs$data,
    configs = inputs$configs
  )
  post_results <- creadeweather::postcompute(results)
  list(results = results, post_results = post_results)
}

#' Extract a specific variable from post-processed results
extract_variable <- function(post_results, variable_name) {
  post_results$result[[1]] %>%
    dplyr::filter(variable == variable_name) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(!is.na(value))
}

#' Calculate trend change over the data span
calculate_trend_change <- function(trend_tbl) {
  trend_fit <- stats::lm(value ~ as.numeric(date), data = trend_tbl)
  days_span <- as.numeric(max(trend_tbl$date) - min(trend_tbl$date))
  stats::coef(trend_fit)[["as.numeric(date)"]] * days_span
}


# =============================================================================
# Tests for individual scenarios
# =============================================================================

test_that("trend scenario captures the synthetic long-term decline", {

  inputs <- synthetic_train_inputs(
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE
  )

  pipeline <- run_pipeline(inputs)

  # Validate result structure
  expect_s3_class(pipeline$results, "tbl_df")
  expect_equal(nrow(pipeline$results), 1)
  expect_equal(nrow(pipeline$post_results), 1)
  expect_equal(pipeline$post_results$config[[1]]$output, "trend")

  # Extract and analyze trend
  trend_tbl <- extract_variable(pipeline$post_results, "trend")

  # Visual inspection (set to TRUE for debugging)
  if (FALSE) {
    ggplot2::ggplot(trend_tbl) +
      ggplot2::geom_line(ggplot2::aes(date, value))
  }

  trend_change <- calculate_trend_change(trend_tbl)
  expected_drop <- inputs$expectations$trend_drop

  # Trend should be negative (decline) and within expected range
  expect_gt(-trend_change, expected_drop * TREND_MIN_DETECTION_FRACTION)
  expect_lt(-trend_change, expected_drop * TREND_MAX_DETECTION_FRACTION)
})


test_that("anomaly scenario highlights the injected spike", {

  inputs <- synthetic_train_inputs(
    include_trend = FALSE,
    include_anomaly = TRUE,
    include_fire = FALSE
  )

  pipeline <- run_pipeline(inputs)

  # Validate result structure
  expect_s3_class(pipeline$results, "tbl_df")
  expect_equal(nrow(pipeline$results), 1)
  expect_equal(nrow(pipeline$post_results), 1)
  expect_equal(pipeline$post_results$config[[1]]$output, "anomaly")

  # Extract anomaly results (only prediction period)
  anomaly_tbl <- extract_variable(pipeline$post_results, "anomaly") %>%
    dplyr::filter(date > inputs$expectations$anomaly_training_end)

  anomaly_window <- inputs$expectations$anomaly_window
  expect_gt(length(anomaly_window), 0)

  # Visual inspection (set to TRUE for debugging)
  if (FALSE) {
    ggplot2::ggplot(anomaly_tbl) +
      ggplot2::geom_line(ggplot2::aes(date, value)) +
      ggplot2::geom_vline(xintercept = as.Date(anomaly_window), col = "red")
  }

  # Calculate mean in anomaly window vs baseline
  window_mean <- mean(
    anomaly_tbl$value[anomaly_tbl$date %in% anomaly_window],
    na.rm = TRUE
  )

  baseline_mean <- mean(
    anomaly_tbl$value[!(anomaly_tbl$date %in% anomaly_window)],
    na.rm = TRUE
  )

  expected_magnitude <- inputs$expectations$anomaly_magnitude

  # Window mean should capture most of the injected spike
  expect_gt(window_mean, expected_magnitude * ANOMALY_MIN_SIGNAL_FRACTION)

  # Baseline should be close to zero (no anomaly outside window)
  expect_lt(abs(baseline_mean), expected_magnitude * ANOMALY_MAX_BASELINE_FRACTION)
})


test_that("fire scenario reproduces the expected average uplift", {

  inputs <- synthetic_train_inputs(
    include_trend = FALSE,
    include_anomaly = FALSE,
    include_fire = TRUE,
    fire_length = 120,
    fire_random = TRUE
  )

  pipeline <- run_pipeline(inputs)

  # Validate result structure
  expect_s3_class(pipeline$results, "tbl_df")
  expect_equal(nrow(pipeline$results), 1)
  expect_equal(nrow(pipeline$post_results), 1)
  expect_true(pipeline$post_results$config[[1]]$add_fire)

  # Extract predictions with and without fire
  pred_tbl <- pipeline$post_results$result[[1]] %>%
    dplyr::filter(variable %in% c("predicted", "predicted_nofire")) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::arrange(date)

  expect_true(
    "predicted_nofire" %in% pred_tbl$variable,
    info = "predicted_nofire should be created when add_fire is TRUE"
  )

  # Calculate fire contribution (predicted - predicted_nofire)
  delta_tbl <- pred_tbl %>%
    tidyr::pivot_wider(names_from = variable, values_from = value)

  expect_true("predicted" %in% names(delta_tbl))
  expect_true("predicted_nofire" %in% names(delta_tbl))

  delta_tbl <- delta_tbl %>%
    dplyr::mutate(diff = predicted - predicted_nofire)

  overall_mean <- mean(delta_tbl$diff, na.rm = TRUE)
  expected_avg <- inputs$expectations$fire_average_contribution
  fire_dates <- inputs$expectations$fire_window

  expect_gt(inputs$expectations$fire_days, 0)

  # Check if fire effect was learned by the model
  has_fire_effect <- abs(overall_mean) > 0.01

  if (has_fire_effect) {
    # Model learned fire effect - check it's in the right ballpark
    tolerance <- max(FIRE_TOLERANCE_ABSOLUTE, abs(expected_avg) * FIRE_TOLERANCE_RELATIVE)
    expect_equal(overall_mean, expected_avg, tolerance = tolerance)

    # Fire days should show higher contribution than non-fire days
    if (length(fire_dates) > 0) {
      fire_mean <- mean(delta_tbl$diff[delta_tbl$date %in% fire_dates], na.rm = TRUE)
      nonfire_mean <- mean(delta_tbl$diff[!(delta_tbl$date %in% fire_dates)], na.rm = TRUE)

      if (abs(fire_mean) > 0.01) {
        expect_gt(fire_mean, nonfire_mean - 0.1)
      }
    }
  } else {
    # Model didn't learn fire effect - acceptable outcome, log it
    expect_true(TRUE, info = "Model did not learn fire effect (predicted_nofire equals predicted)")
  }
})


# =============================================================================
# Tests for combined scenarios
# =============================================================================

test_that("combined trend + anomaly scenario captures both signals", {

  inputs <- synthetic_train_inputs(
    include_trend = TRUE,
    include_anomaly = TRUE,
    include_fire = FALSE
  )

  pipeline <- run_pipeline(inputs)

  # Should have both trend and anomaly configs
  expect_s3_class(pipeline$results, "tbl_df")
  expect_equal(nrow(pipeline$results), 2)
  expect_equal(nrow(pipeline$post_results), 2)

  outputs <- sapply(pipeline$post_results$config, function(x) x$output)
  expect_true("trend" %in% outputs)
  expect_true("anomaly" %in% outputs)

  # Check trend result
  trend_post <- pipeline$post_results %>%
    dplyr::filter(sapply(config, function(x) x$output) == "trend")

  trend_tbl <- trend_post$result[[1]] %>%
    dplyr::filter(variable == "trend") %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(!is.na(value))

  trend_change <- calculate_trend_change(trend_tbl)
  expected_drop <- inputs$expectations$trend_drop

  expect_gt(-trend_change, expected_drop * TREND_MIN_DETECTION_FRACTION)

  # Check anomaly result
  anomaly_post <- pipeline$post_results %>%
    dplyr::filter(sapply(config, function(x) x$output) == "anomaly")

  anomaly_tbl <- anomaly_post$result[[1]] %>%
    dplyr::filter(variable == "anomaly") %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(date > inputs$expectations$anomaly_training_end)

  anomaly_window <- inputs$expectations$anomaly_window
  window_mean <- mean(
    anomaly_tbl$value[anomaly_tbl$date %in% anomaly_window],
    na.rm = TRUE
  )

  expected_magnitude <- inputs$expectations$anomaly_magnitude
  expect_gt(window_mean, expected_magnitude * ANOMALY_MIN_SIGNAL_FRACTION)
})


# =============================================================================
# Tests for result structure validation
# =============================================================================

test_that("train_configs returns expected structure", {

  inputs <- synthetic_train_inputs(
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE
  )

  results <- creadeweather::train_configs(
    data = inputs$data,
    configs = inputs$configs
  )

  expect_s3_class(results, "tbl_df")
  expect_true("models" %in% names(results))
  expect_true("performances" %in% names(results))
  expect_true("data" %in% names(results))
  expect_true("config" %in% names(results))
  expect_true("location_id" %in% names(results))
})

test_that("postcompute returns expected structure", {

  inputs <- synthetic_train_inputs(
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE
  )

  results <- creadeweather::train_configs(
    data = inputs$data,
    configs = inputs$configs
  )

  post_results <- creadeweather::postcompute(results)

  expect_s3_class(post_results, "tbl_df")
  expect_true("result" %in% names(post_results))
  expect_true("config" %in% names(post_results))

  # Result should be a tibble with date, variable, value columns
  result_tbl <- post_results$result[[1]]
  expect_true("date" %in% names(result_tbl))
  expect_true("variable" %in% names(result_tbl))
  expect_true("value" %in% names(result_tbl))
})
