# Unit tests for postcompute_gbm functions that include
# - postcompute_gbm_lighten_model
# - postcompute_gbm_fire
# - postcompute_gbm_trend
# and the overall postcompute_gbm function


library(testthat)
library(dplyr)
library(purrr)
library(lubridate)


testthat::source_test_helpers("tests/testthat", env = globalenv())


test_that("postcompute_gbm_lighten_model returns compact summary with metrics", {
  
  fixture <- build_gbm_fixture(
    output = "trend",
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE,
    training_days = 120,
    trees = 60
  )

  
  # Add predicted column
  data <- fixture$data
  data$predicted <- gbm::predict.gbm(fixture$model, data)
  
  light <- creadeweather:::postcompute_gbm_lighten_model(
    model = fixture$model,
    data = data
  )

  expect_true(all(c("shrinkage", "train.fraction", "cv.folds", "importance") %in% names(light)))
  expect_true(any(grepl("^rmse_", names(light))))
  expect_true(any(grepl("^rsquared_", names(light))))
  expect_true(is.data.frame(light$importance))
  expect_true("rmse_training" %in% names(light))
  expect_true(is.numeric(light$rmse_training))
  expect_true(all(light$rmse_training >= 0, na.rm = TRUE))

  # Object size should be smaller
  expect_lt(object.size(light), object.size(fixture$model))

})



test_that("postcompute_gbm_fire creates counterfactual predictions", {
  
  fixture <- build_gbm_fixture(
    output = "anomaly",
    include_trend = FALSE,
    include_anomaly = FALSE,
    include_fire = TRUE,
    training_days = 365,
    trees = 100
  )

  expect_true(any(fixture$data$fire_frp > 0))
  
  # Add predicted column
  data <- fixture$data
  data$predicted <- gbm::predict.gbm(fixture$model, data)

  enriched <- creadeweather:::postcompute_gbm_fire(
    data = data,
    model = fixture$model,
    do_unlink = fixture$do_unlink,
    weather_vars = fixture$config$weather_vars
  )

  expect_true("predicted_nofire" %in% names(enriched))

  fire_rows <- enriched$fire_frp > 0
  expect_true(any(fire_rows))

  nofire_rows <- enriched$fire_frp == 0
  expect_equal(
    enriched$predicted[nofire_rows],
    enriched$predicted_nofire[nofire_rows],
    tolerance = 1e-6
  )

  # For rows with fire, the predicted_nofire should be different from the predicted
  fire_diff <- enriched$predicted[fire_rows] - enriched$predicted_nofire[fire_rows]
  expect_true(all(fire_diff > 1))
  
  # Check average contribution
  avg_contribution <- mean(enriched$predicted - enriched$predicted_nofire)
  fixture$inputs$expectations$fire_average_contribution
  expect_equal(
    avg_contribution,
    fixture$inputs$expectations$fire_average_contribution,
    tolerance = 0.1
  )
})


test_that("postcompute_gbm_trend aggregates partial dependencies into trend column", {
  
  fixture <- build_gbm_fixture(
    output = "trend",
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE,
    training_days = 140,
    trees = 60
  )

  augmented <- suppressWarnings(
    creadeweather:::postcompute_gbm_trend(
      data = fixture$data,
      time_vars = fixture$config$time_vars,
      model = fixture$model,
      do_unlink = fixture$do_unlink
    )
  )

  expect_true("trend" %in% names(augmented))
  expect_equal(nrow(augmented), nrow(fixture$data))
  expect_true(any(!is.na(augmented$trend)))

  training_rows <- augmented$set == "training"
  expect_true(any(training_rows))
  expect_true(sd(augmented$trend[training_rows], na.rm = TRUE) > 0)
})


test_that("postcompute_gbm returns tidy results", {
  
  fixture <- build_gbm_fixture(
    output = "trend",
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE,
    training_days = 120,
    trees = 60
  )

  result_tbl <- suppressWarnings(
    creadeweather:::postcompute_gbm(
      model = fixture$model,
      data = fixture$data,
      config = fixture$config
    )
  )

  expect_s3_class(result_tbl, "tbl_df")
  expect_equal(nrow(result_tbl), 1)
  expect_true(all(c("config", "model", "result") %in% names(result_tbl)))

  light_model <- result_tbl$model[[1]]
  expect_true(all(c("shrinkage", "train.fraction", "cv.folds") %in% names(light_model)))

  long_result <- result_tbl$result[[1]]
  expect_s3_class(long_result, "tbl_df")
  expect_true(all(c("date", "variable", "value") %in% names(long_result)))

  wide <- long_result %>%
    tidyr::pivot_wider(names_from = variable, values_from = value)

  expect_true(all(c("predicted", "observed", "anomaly") %in% names(wide)))
  expect_equal(
    wide$observed - wide$predicted,
    wide$anomaly,
    tolerance = 1e-6
  )
})


test_that("postcompute_gbm applies log link correctly", {
  
  fixture <- build_gbm_fixture(
    output = "anomaly",
    include_trend = FALSE,
    include_anomaly = TRUE,
    include_fire = FALSE,
    link_override = "log",
    training_days = 160,
    trees = 60
  )


  result_tbl <- suppressWarnings(
    creadeweather:::postcompute_gbm(
      model = fixture$model,
      data = fixture$data,
      config = fixture$config
    )
  )
  
  long_result <- result_tbl$result[[1]]
  do_unlink <- fixture$do_unlink
  
  avg_observed <- mean(long_result[long_result$variable == "observed", ]$value, na.rm = TRUE)  
  avg_value <- mean(do_unlink(fixture$data$value))
  expect_equal(avg_observed, avg_value, tolerance = 1e-6)
})


