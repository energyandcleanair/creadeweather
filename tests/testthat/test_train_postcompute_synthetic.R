# This is a high-level series of tests that validate that 
# our training + postcompute can capture meaningful signals from synthetic data.
# 
# We test the following scenarios:
# - Trend: a long-term decline in the data
# - Anomaly: a spike in the data
# - Fire: a series of fires in the data

library(testthat)
library(dplyr)
library(purrr)
library(lubridate)

if (!("creadeweather" %in% loadedNamespaces())) {
  devtools::load_all(".")
}

testthat::source_test_helpers("tests/testthat", env = globalenv())

test_that("trend scenario captures the synthetic long-term decline", {
  
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
  
  # Check format of results and post results
  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 1)
  
  expect_equal(nrow(post_results), 1)
  expect_equal(post_results$config[[1]]$output, "trend")

  trend_tbl <- post_results$result[[1]] %>%
    dplyr::filter(variable == "trend") %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(!is.na(value))
  
  # Visual inspection
  if(F){
    ggplot(trend_tbl) +
      geom_line(aes(date, value))
  }

  trend_fit <- stats::lm(value ~ as.numeric(date), data = trend_tbl)
  days_span <- as.numeric(max(trend_tbl$date) - min(trend_tbl$date))
  trend_change <- stats::coef(trend_fit)[["as.numeric(date)"]] * days_span

  expect_gt(-trend_change, inputs$expectations$trend_drop * 0.4)
  expect_lt(-trend_change, inputs$expectations$trend_drop * 1.5)
})


test_that("anomaly scenario highlights the injected spike", {
  
  inputs <- synthetic_train_inputs(
    include_trend = FALSE,
    include_anomaly = TRUE,
    include_fire = FALSE
  )

  results <- creadeweather::train_configs(
    data = inputs$data,
    configs = inputs$configs
  )
  post_results <- creadeweather::postcompute(results)
  
  
  # Check format of results and post results
  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 1)
  
  expect_equal(nrow(post_results), 1)
  expect_equal(post_results$config[[1]]$output, "anomaly")

  # Extracting anomaly results
  anomaly_tbl <- post_results$result[[1]] %>%
    dplyr::filter(variable == "anomaly") %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(date > inputs$expectations$anomaly_training_end)
  
  anomaly_window <- inputs$expectations$anomaly_window
  expect_gt(length(anomaly_window), 0)
  
  # Visual inspection
  if(F){
    ggplot(anomaly_tbl) +
      geom_line(aes(date, value)) +
      geom_vline(xintercept = as.Date(anomaly_window), col='red')
  }

  # Mean in anomaly window should be significantly higher than baseline
  window_mean <- mean(
    anomaly_tbl$value[anomaly_tbl$date %in% anomaly_window],
    na.rm = TRUE
  )

  baseline_mean <- mean(
    anomaly_tbl$value[!(anomaly_tbl$date %in% anomaly_window)],
    na.rm = TRUE
  )

  expect_gt(window_mean, inputs$expectations$anomaly_magnitude * 0.5)
  expect_lt(abs(baseline_mean), inputs$expectations$anomaly_magnitude * 0.2)
})


test_that("fire scenario reproduces the expected average uplift", {
  
  inputs <- synthetic_train_inputs(
    include_trend = FALSE,
    include_anomaly = FALSE,
    include_fire = TRUE,
    fire_length = 120,
    fire_random = TRUE
  )

  results <- creadeweather::train_configs(
    data = inputs$data,
    configs = inputs$configs
  )
  
  post_results <- creadeweather::postcompute(results)

  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 1)
  expect_equal(nrow(post_results), 1)
  expect_true(post_results$config[[1]]$add_fire)

  pred_tbl <- post_results$result[[1]] %>%
    dplyr::filter(variable %in% c("predicted", "predicted_nofire")) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::arrange(date)

  delta_tbl <- pred_tbl %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    dplyr::mutate(diff = predicted - predicted_nofire)

  overall_mean <- mean(delta_tbl$diff, na.rm = TRUE)
  expected_avg <- inputs$expectations$fire_average_contribution

  expect_gt(inputs$expectations$fire_days, 0)
  expect_equal(
    overall_mean,
    expected_avg,
    tolerance = max(0.1, abs(expected_avg) * 0.2)
  )

  fire_dates <- inputs$expectations$fire_window
  if (length(fire_dates) > 0) {
    fire_mean <- mean(delta_tbl$diff[delta_tbl$date %in% fire_dates], na.rm = TRUE)
    nonfire_mean <- mean(delta_tbl$diff[!(delta_tbl$date %in% fire_dates)], na.rm = TRUE)
    expect_gt(fire_mean, nonfire_mean)
    expect_lt(abs(nonfire_mean), max(0.05, expected_avg * 0.1))
  }
})

