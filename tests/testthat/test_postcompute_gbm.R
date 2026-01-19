# =============================================================================
# Unit tests for postcompute_gbm module (R/04_postcompute_gbm.R)
#
# Tests the post-processing of GBM training results:
# - postcompute_gbm_lighten_model(): Model size reduction
# - postcompute_gbm_fire(): Fire counterfactual computation
# - postcompute_gbm_trends(): Time trend extraction
# - postcompute_gbm(): Full post-processing pipeline
# =============================================================================

testthat::source_test_helpers("tests/testthat", env = globalenv())


# =============================================================================
# Tests for postcompute_gbm_lighten_model()
# =============================================================================

test_that("postcompute_gbm_lighten_model returns compact summary", {

  fixture <- build_gbm_fixture(
    output = "trend",
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE,
    training_days = 120,
    trees = 60
  )

  data <- fixture$data
  data$predicted <- predict(fixture$model, data, n.trees = fixture$model$n.trees.opt)

  light <- creadeweather:::postcompute_gbm_lighten_model(
    models = list(fixture$model),
    data = data
  )

  expect_true(all(c("importance", "ntrees_opt") %in% names(light)))
  expect_true(is.list(light$importance))
  expect_true(is.list(light$ntrees_opt))
  expect_equal(length(light$importance), 1)
  expect_equal(length(light$ntrees_opt), 1)
})

test_that("postcompute_gbm_lighten_model handles multiple models", {

  fixture <- build_gbm_fixture(
    output = "trend",
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE,
    training_days = 120,
    trees = 60
  )

  # Simulate multiple bootstrap models
  models <- list(fixture$model, fixture$model)

  light <- creadeweather:::postcompute_gbm_lighten_model(
    models = models,
    data = fixture$data
  )

  expect_equal(length(light$importance), 2)
  expect_equal(length(light$ntrees_opt), 2)
})


# =============================================================================
# Tests for postcompute_gbm_fire()
# =============================================================================

test_that("postcompute_gbm_fire creates predicted_nofire column", {

  fixture <- build_gbm_fixture(
    output = "anomaly",
    include_trend = FALSE,
    include_anomaly = FALSE,
    include_fire = TRUE,
    training_days = 365,
    trees = 100
  )

  expect_true(any(fixture$data$fire_frp > 0))

  data <- fixture$data
  data$predicted <- predict(fixture$model, data, n.trees = fixture$model$n.trees.opt)

  enriched <- creadeweather:::postcompute_gbm_fire(
    data = data,
    models = list(fixture$model),
    do_unlink = fixture$do_unlink,
    weather_vars = fixture$config$weather_vars
  )

  expect_true("predicted_nofire" %in% names(enriched))
})

test_that("postcompute_gbm_fire matches predicted when fire=0", {

  fixture <- build_gbm_fixture(
    output = "anomaly",
    include_trend = FALSE,
    include_anomaly = FALSE,
    include_fire = TRUE,
    training_days = 365,
    trees = 100
  )

  data <- fixture$data
  data$predicted <- predict(fixture$model, data, n.trees = fixture$model$n.trees.opt)

  enriched <- creadeweather:::postcompute_gbm_fire(
    data = data,
    models = list(fixture$model),
    do_unlink = fixture$do_unlink,
    weather_vars = fixture$config$weather_vars
  )

  # For rows without fire, predicted_nofire should equal predicted
  nofire_rows <- enriched$fire_frp == 0
  expect_equal(
    enriched$predicted[nofire_rows],
    enriched$predicted_nofire[nofire_rows],
    tolerance = 1e-6
  )
})

test_that("postcompute_gbm_fire adds a fire effect", {

  fixture <- build_gbm_fixture(
    output = "anomaly",
    include_trend = FALSE,
    include_anomaly = FALSE,
    include_fire = TRUE,
    training_days = 700,
    trees = 1000
  )

  data <- fixture$data
  data$predicted <- predict(fixture$model, data, n.trees = fixture$model$n.trees.opt)

  enriched <- creadeweather:::postcompute_gbm_fire(
    data = data,
    models = list(fixture$model),
    do_unlink = fixture$do_unlink,
    weather_vars = fixture$config$weather_vars
  )

  fire_rows <- enriched$fire_frp > 0
  expect_true(any(fire_rows))

  # For dire days, there should be a difference between predicted and predicted_nofire
  fire_diff <- enriched$predicted[fire_rows] - enriched$predicted_nofire[fire_rows]
  expect_true(any(fire_diff != 0))
})


# =============================================================================
# Tests for postcompute_gbm_trends()
# =============================================================================

test_that("postcompute_gbm_trends adds trend column", {

  fixture <- build_gbm_fixture(
    output = "trend",
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE,
    training_days = 140,
    trees = 60
  )

  augmented <- suppressWarnings(
    creadeweather:::postcompute_gbm_trends(
      data = fixture$data,
      time_vars = fixture$config$time_vars,
      models = list(fixture$model),
      do_unlink = fixture$do_unlink
    )
  )

  expect_true("trend" %in% names(augmented))
  expect_equal(nrow(augmented), nrow(fixture$data))
  expect_true(any(!is.na(augmented$trend)))
})

test_that("postcompute_gbm_trends shows variation over time", {

  fixture <- build_gbm_fixture(
    output = "trend",
    include_trend = TRUE,
    include_anomaly = FALSE,
    include_fire = FALSE,
    training_days = 140,
    trees = 60
  )

  augmented <- suppressWarnings(
    creadeweather:::postcompute_gbm_trends(
      data = fixture$data,
      time_vars = fixture$config$time_vars,
      models = list(fixture$model),
      do_unlink = fixture$do_unlink
    )
  )

  # Trend should vary across training period
  training_rows <- augmented$set == "training"
  expect_true(any(training_rows))
  expect_true(sd(augmented$trend[training_rows], na.rm = TRUE) > 0)
})


# =============================================================================
# Tests for postcompute_gbm() - full pipeline
# =============================================================================

test_that("postcompute_gbm returns tidy results structure", {

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
      models = list(fixture$model),
      data = fixture$data,
      config = fixture$config,
      performances = list()
    )
  )

  expect_s3_class(result_tbl, "tbl_df")
  expect_equal(nrow(result_tbl), 1)
  expect_true(all(c("config", "models", "result", "performances") %in% names(result_tbl)))
})

test_that("postcompute_gbm creates lightened model", {

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
      models = list(fixture$model),
      data = fixture$data,
      config = fixture$config,
      performances = list()
    )
  )

  light_model <- result_tbl$models[[1]]
  expect_true(all(c("importance", "ntrees_opt") %in% names(light_model)))
})

test_that("postcompute_gbm produces long-format results", {

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
      models = list(fixture$model),
      data = fixture$data,
      config = fixture$config,
      performances = list()
    )
  )

  long_result <- result_tbl$result[[1]]
  expect_s3_class(long_result, "tbl_df")
  expect_true(all(c("date", "variable", "value") %in% names(long_result)))
})

test_that("postcompute_gbm computes anomaly correctly", {

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
      models = list(fixture$model),
      data = fixture$data,
      config = fixture$config,
      performances = list()
    )
  )

  long_result <- result_tbl$result[[1]]
  wide <- long_result %>%
    tidyr::pivot_wider(names_from = variable, values_from = value)

  expect_true(all(c("predicted", "observed", "anomaly") %in% names(wide)))

  # anomaly = observed - predicted
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
      models = list(fixture$model),
      data = fixture$data,
      config = fixture$config,
      performances = list()
    )
  )

  long_result <- result_tbl$result[[1]]
  do_unlink <- fixture$do_unlink

  # Observed values should be back-transformed
  avg_observed <- mean(
    long_result[long_result$variable == "observed", ]$value,
    na.rm = TRUE
  )
  avg_value <- mean(do_unlink(fixture$data$value))

  expect_equal(avg_observed, avg_value, tolerance = 1e-6)
})
