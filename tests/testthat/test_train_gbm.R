# =============================================================================
# Unit tests for train_gbm module (R/03_train_gbm.R)
#
# Tests the GBM training pipeline:
# - train_gbm_prepare_data(): Data partitioning, link functions, excluded dates
# - train_gbm_fit_model(): Model fitting and prediction generation
# - get_model_performance(): Performance metrics calculation
# - train_gbm(): Full pipeline integration
# =============================================================================

testthat::source_test_helpers("tests/testthat", env = globalenv())


# =============================================================================
# Test fixtures
# =============================================================================

#' Create standard inputs for GBM tests using synthetic data
create_gbm_test_inputs <- function(training_days = 180, prediction_days = 30) {
  synthetic_train_inputs(
    training_days = training_days,
    prediction_days = prediction_days,
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE
  )
}

#' Extract the trend config from inputs
get_trend_config <- function(inputs) {
  inputs$configs %>% dplyr::filter(output == "trend")
}


# =============================================================================
# Tests for train_gbm_prepare_data()
# =============================================================================

test_that("train_gbm_prepare_data partitions data into training/testing/prediction sets", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

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
  expect_true(all(prep$data$set %in% c("training", "testing", "prediction")))
  expect_gt(sum(prep$data$set == "training"), 0)
  expect_gt(sum(prep$data$set == "testing"), 0)
  expect_gt(sum(prep$data$set == "prediction"), 0)
})

test_that("train_gbm_prepare_data returns formula and link functions", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]],
    link = config$link[[1]],
    training.fraction = config$training.fraction[[1]]
  )

  expect_true(inherits(prep$formula, "formula"))
  expect_true(is.function(prep$do_link))
  expect_true(is.function(prep$do_unlink))
})

test_that("train_gbm_prepare_data respects training_excluded_dates", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  # Exclude a specific date that should be in training period
  excluded_date <- as.Date(config$training_end[[1]]) - 5

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]],
    link = config$link[[1]],
    training.fraction = config$training.fraction[[1]],
    training_excluded_dates = excluded_date
  )

  excluded_rows <- prep$data[as.Date(prep$data$date) == excluded_date, , drop = FALSE]

  expect_gt(nrow(excluded_rows), 0)
  expect_true(all(excluded_rows$set != "training"))
  expect_true(any(excluded_rows$set == "testing"))
})

test_that("train_gbm_prepare_data with link='linear' preserves values", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)
  original_values <- inputs$data$meas_weather[[1]]$value

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]],
    link = "linear",
    training.fraction = 1
  )

  # Linear link should preserve values (identity function)
  expect_equal(prep$do_link(10), 10)
  expect_equal(prep$do_unlink(10), 10)
})

test_that("train_gbm_prepare_data with link='log' transforms values", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]],
    link = "log",
    training.fraction = 1
  )

  # Log link should apply log/exp transforms
  expect_equal(prep$do_link(10), log(10))
  expect_equal(prep$do_unlink(log(10)), 10)

  # Values in data should be log-transformed
  expect_true(all(prep$data$value < 10))  # Original values ~30, log(30) ~3.4
})

test_that("train_gbm_prepare_data rejects invalid link", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  expect_error(
    creadeweather:::train_gbm_prepare_data(
      data = inputs$data$meas_weather[[1]],
      training_end = config$training_end[[1]],
      weather_vars = config$weather_vars[[1]],
      time_vars = config$time_vars[[1]],
      link = "invalid"
    ),
    "link must be"
  )
})

test_that("train_gbm_prepare_data errors when no training data available", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  # Set training_end before all data
  expect_error(
    creadeweather:::train_gbm_prepare_data(
      data = inputs$data$meas_weather[[1]],
      training_end = "2020-01-01",  # Before synthetic data starts
      weather_vars = config$weather_vars[[1]],
      time_vars = config$time_vars[[1]]
    ),
    "No data available before training_end"
  )
})

test_that("train_gbm_prepare_data respects training.fraction", {
  inputs <- create_gbm_test_inputs(training_days = 100, prediction_days = 0)
  config <- get_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]],
    training.fraction = 0.7
  )

  n_training <- sum(prep$data$set == "training")
  n_testing <- sum(prep$data$set == "testing")
  total <- n_training + n_testing

  # Training should be approximately 70% of pre-cutoff data
  expect_equal(n_training / total, 0.7, tolerance = 0.05)
})

test_that("train_gbm_prepare_data handles NULL training_end", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = NULL,  # Should default to far future
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]]
  )

  # All data should be in training/testing (none in prediction)
  expect_equal(sum(prep$data$set == "prediction"), 0)
})


# =============================================================================
# Tests for train_gbm_fit_model()
# =============================================================================

test_that("train_gbm_fit_model trains a GBM model", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

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
    trees = 50,
    interaction.depth = 1,
    learning.rate = 0.1,
    cv_folds = 2
  )

  expect_s3_class(fit, "tbl_df")
  expect_equal(nrow(fit), 1)
  expect_true(inherits(fit$model[[1]], "GBMFit"))
})

test_that("train_gbm_fit_model adds predictions to data", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]]
  )

  fit <- creadeweather:::train_gbm_fit_model(
    data_prepared = prep$data,
    formula = prep$formula,
    trees = 50,
    cv_folds = 2
  )

  expect_true("predicted" %in% names(fit$data[[1]]))
  expect_equal(nrow(fit$data[[1]]), nrow(prep$data))
  expect_false(any(is.na(fit$data[[1]]$predicted)))
})

test_that("train_gbm_fit_model returns performance metrics", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]]
  )

  fit <- creadeweather:::train_gbm_fit_model(
    data_prepared = prep$data,
    formula = prep$formula,
    trees = 50,
    cv_folds = 2
  )

  perf <- fit$performance[[1]]

  expect_true("rmse_training" %in% names(perf))
  expect_true("rsquared_training" %in% names(perf))
  expect_true(is.numeric(perf$rmse_training))
  expect_true(perf$rsquared_training >= 0 && perf$rsquared_training <= 1)
})

test_that("train_gbm_fit_model stores optimal tree count", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]]
  )

  fit <- creadeweather:::train_gbm_fit_model(
    data_prepared = prep$data,
    formula = prep$formula,
    trees = 100,
    cv_folds = 2
  )

  model <- fit$model[[1]]
  expect_true("n.trees.opt" %in% names(model))
  expect_true(model$n.trees.opt > 0)
  expect_true(model$n.trees.opt <= 100)
})

test_that("train_gbm_fit_model errors without set column", {
  inputs <- create_gbm_test_inputs()

  bad_data <- inputs$data$meas_weather[[1]]
  # No 'set' column

  expect_error(
    creadeweather:::train_gbm_fit_model(
      data_prepared = bad_data,
      formula = value ~ ws + wd,
      trees = 50
    ),
    "must include a 'set' column"
  )
})


# =============================================================================
# Tests for get_model_performance()
# =============================================================================

test_that("get_model_performance calculates metrics for each set", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]]
  )

  fit <- creadeweather:::train_gbm_fit_model(
    data_prepared = prep$data,
    formula = prep$formula,
    trees = 50,
    cv_folds = 2
  )

  perf <- creadeweather:::get_model_performance(fit$model[[1]], fit$data[[1]])

  # Should have metrics for all sets
  expect_true("rmse_training" %in% names(perf))
  expect_true("rmse_testing" %in% names(perf))
  expect_true("rmse_prediction" %in% names(perf))
  expect_true("rsquared_training" %in% names(perf))
  expect_true("rsquared_testing" %in% names(perf))
  expect_true("rsquared_prediction" %in% names(perf))
})

test_that("get_model_performance returns valid R² values", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  prep <- creadeweather:::train_gbm_prepare_data(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]]
  )

  fit <- creadeweather:::train_gbm_fit_model(
    data_prepared = prep$data,
    formula = prep$formula,
    trees = 50,
    cv_folds = 2
  )

  perf <- fit$performance[[1]]

  # R² should be between 0 and 1
  expect_true(perf$rsquared_training >= 0 && perf$rsquared_training <= 1)
  expect_true(perf$rsquared_testing >= 0 && perf$rsquared_testing <= 1)
})


# =============================================================================
# Tests for train_gbm() - integration
# =============================================================================

test_that("train_gbm runs full pipeline successfully", {
  inputs <- create_gbm_test_inputs()
  config <- get_trend_config(inputs)

  result <- train_gbm(
    data = inputs$data$meas_weather[[1]],
    training_end = config$training_end[[1]],
    weather_vars = config$weather_vars[[1]],
    time_vars = config$time_vars[[1]],
    trees = 50,
    normalise = FALSE,
    detect_breaks = FALSE,
    samples = 100,
    training.fraction = 0.8,
    cv_folds = 2,
    parallel = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_true(inherits(result$model[[1]], "GBMFit"))
  expect_true("predicted" %in% names(result$data[[1]]))
  expect_true(is.list(result$performance[[1]]))
})
