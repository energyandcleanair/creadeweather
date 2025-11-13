# Unit tests for train_gbm helpers and wrapper using synthetic scenarios.

library(testthat)
library(dplyr)
library(lubridate)

if (!("creadeweather" %in% loadedNamespaces())) {
  devtools::load_all(".")
}

testthat::source_test_helpers("tests/testthat", env = globalenv())

suppressPackageStartupMessages(library(gbm))

trend_inputs_for_gbm <- function() {
  synthetic_train_inputs(
    training_days = 180,
    prediction_days = 30,
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE
  )
}

extract_trend_config <- function(inputs) {
  inputs$configs %>%
    dplyr::filter(output == "trend")
}


test_that("train_gbm_prepare_data partitions and filters correctly", {
  inputs <- trend_inputs_for_gbm()
  config <- extract_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]],
    link = config$link[[1]],
    training.fraction = config$training.fraction[[1]]
  )

  expect_s3_class(prep$data, "data.frame")
  expect_true(all(c("set", "value") %in% names(prep$data)))
  expect_gt(sum(prep$data$set == "training"), 0)
  expect_gt(sum(prep$data$set == "testing"), 0)
  expect_gt(sum(prep$data$set == "prediction"), 0)
  expect_true(inherits(prep$formula, "formula"))
  expect_true(is.function(prep$do_link))
  expect_true(is.function(prep$do_unlink))
})


test_that("train_gbm_fit_model trains a gbm model", {
  inputs <- trend_inputs_for_gbm()
  config <- extract_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]],
    link = config$link[[1]],
    training.fraction = config$training.fraction[[1]]
  )

  fit <- creadeweather:::train_gbm_fit_model(
    data_prepared = prep$data,
    formula = prep$formula,
    trees = 60,
    interaction.depth = config$interaction.depth[[1]],
    learning.rate = config$learning.rate[[1]],
    cv_folds = config$cv_folds[[1]]
  )

  expect_s3_class(fit, "tbl_df")
  expect_equal(nrow(fit), 1)
  expect_s3_class(fit$model[[1]], "gbm")
  expect_equal(fit$data[[1]], prep$data)
})
