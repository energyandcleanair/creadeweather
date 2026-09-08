# Tests for parallel training (R/03_train_parallel.R and train_models()).
#
# The contract these protect: parallelising the outer loop over models must
# not change any result. train_models() sets the seed per model inside the
# worker, so output must not depend on the number of workers, the backend, or
# the order rows happen to finish in.

test_that("dw_split_cores prefers processes and spends leftovers on threads", {
  # More models than cores: everything goes to processes, gbm stays single
  # threaded so that workers x threads does not oversubscribe the box.
  s <- creadeweather:::dw_split_cores(n_items = 93, n_cores = 4)
  expect_equal(s$workers, 4L)
  expect_equal(s$threads, 1L)

  # Fewer models than cores (deweather() called one location at a time):
  # the spare cores are handed to gbm3's own threading instead of idling.
  s <- creadeweather:::dw_split_cores(n_items = 3, n_cores = 8)
  expect_equal(s$workers, 3L)
  expect_equal(s$threads, 2L)

  # A single model still uses the whole budget.
  s <- creadeweather:::dw_split_cores(n_items = 1, n_cores = 8)
  expect_equal(s$workers, 1L)
  expect_equal(s$threads, 8L)

  # Never allocate more than we have, and never go below one.
  s <- creadeweather:::dw_split_cores(n_items = 10, n_cores = 1)
  expect_equal(s$workers, 1L)
  expect_equal(s$threads, 1L)
})


test_that("dw_split_cores never exceeds the core budget", {
  for (cores in c(1, 2, 4, 8, 16)) {
    for (items in c(1, 2, 3, 7, 93)) {
      s <- creadeweather:::dw_split_cores(n_items = items, n_cores = cores)
      expect_lte(s$workers, cores)
      expect_lte(s$workers, items)
      expect_lte(s$workers * s$threads, cores)
      expect_gte(s$workers, 1L)
      expect_gte(s$threads, 1L)
    }
  }
})


test_that("dw_parallel_backend returns a serial backend for one worker", {
  b <- creadeweather:::dw_parallel_backend(1)
  expect_null(b$cl)
  expect_silent(b$close())
})


test_that("dw_parallel_backend returns a usable backend and closes cleanly", {
  b <- creadeweather:::dw_parallel_backend(2)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  if (.Platform$OS.type == "windows") {
    # Windows cannot fork, so we need a real cluster object.
    expect_true(inherits(b$cl, "cluster"))
  } else {
    # Unix forks, for which pbapply takes a plain worker count.
    expect_true(is.numeric(b$cl))
    expect_equal(as.integer(b$cl), 2L)
  }
  expect_silent(b$close())
})


test_that("parallel training gives identical results to serial training", {
  weather_vars <- c(
    "air_temp_min", "air_temp_max", "atmos_pres", "wd", "ws",
    "precip", "dewpoint_temp", "pbl_min", "pbl_max"
  )

  # Several independent models, as train_models() sees them when deweather()
  # is given more than one location or pollutant.
  n_models <- 4
  data <- tibble::tibble(
    location_id = paste0("city_", rep(seq_len(2), each = 2)),
    poll = rep(c("pm25", "no2"), 2),
    unit = "ug/m3",
    source = "test",
    process_id = "test_process",
    meas_weather = lapply(seq_len(n_models), function(i) {
      synthetic_train_inputs(seed = 100 + i)$data$meas_weather[[1]]
    })
  )

  train <- function(n_cores) {
    # Deliberately differing outer RNG state: results must not depend on it,
    # because the seed is re-set per model inside the worker.
    set.seed(n_cores * 7919)
    creadeweather::train_models(
      data = data,
      engine = "gbm",
      trees = 300,
      weather_vars = weather_vars,
      time_vars = c(),
      training_end = "2099-01-01",
      training.fraction = 1,
      lag = 0,
      interaction.depth = 7,
      learning.rate = 0.01,
      cv_folds = 3,
      link = "linear",
      n_cores = n_cores
    )
  }

  serial <- train(1)
  parallel_res <- train(n_models)

  expect_equal(nrow(serial), nrow(parallel_res))
  # Row order must be preserved, not just the set of rows.
  expect_identical(serial$location_id, parallel_res$location_id)
  expect_identical(serial$poll, parallel_res$poll)

  predictions <- function(res) lapply(res$data, function(d) d$predicted)
  opt_trees <- function(res) vapply(res$models, function(m) m[[1]]$n.trees.opt, numeric(1))

  # Bit-identical, not merely close.
  expect_identical(predictions(serial), predictions(parallel_res))
  expect_identical(opt_trees(serial), opt_trees(parallel_res))
})


test_that("dw_split_cores reports the budget it resolved", {
  s <- creadeweather:::dw_split_cores(n_items = 93, n_cores = 6)
  expect_equal(s$n_cores, 6L)
  expect_equal(s$requested, 6)

  # With no explicit budget it falls back to availableCores(), and records
  # that the value was auto-detected rather than asked for.
  s <- creadeweather:::dw_split_cores(n_items = 93, n_cores = NULL)
  expect_null(s$requested)
  expect_gte(s$n_cores, 1L)
})


test_that("dw_describe_cores reports usage and both core counts", {
  cores <- creadeweather:::dw_split_cores(n_items = 93, n_cores = 4)
  msg <- creadeweather:::dw_describe_cores(93, cores)

  expect_match(msg, "Training 93 model\\(s\\)")
  expect_match(msg, "using 4 of 4 core\\(s\\)")
  expect_match(msg, "4 worker\\(s\\) x 1 gbm thread\\(s\\)")
  # Both counts are present so a container's CPU limit detection is auditable.
  expect_match(msg, "detectCores=")
  expect_match(msg, "availableCores=")
  expect_match(msg, "n_cores=4")

  # Leftover cores go to threads, and are still counted as used.
  cores <- creadeweather:::dw_split_cores(n_items = 3, n_cores = 8)
  expect_match(creadeweather:::dw_describe_cores(3, cores), "using 6 of 8 core\\(s\\)")
  expect_match(creadeweather:::dw_describe_cores(3, cores), "n_cores=8")

  # Auto-detected budget is labelled as such.
  cores <- creadeweather:::dw_split_cores(n_items = 2, n_cores = NULL)
  expect_match(creadeweather:::dw_describe_cores(2, cores), "n_cores=auto")
})


test_that("train_models logs the cores it uses", {
  weather_vars <- c(
    "air_temp_min", "air_temp_max", "atmos_pres", "wd", "ws",
    "precip", "dewpoint_temp", "pbl_min", "pbl_max"
  )
  data <- tibble::tibble(
    location_id = "city_1",
    poll = "pm25",
    unit = "ug/m3",
    source = "test",
    process_id = "test_process",
    meas_weather = list(synthetic_train_inputs(seed = 7)$data$meas_weather[[1]])
  )

  msgs <- testthat::capture_messages(
    creadeweather::train_models(
      data = data, engine = "gbm", trees = 50, weather_vars = weather_vars,
      time_vars = c(), training_end = "2099-01-01", training.fraction = 1,
      lag = 0, interaction.depth = 7, learning.rate = 0.01, cv_folds = 3,
      link = "linear", n_cores = 2
    )
  )

  expect_true(any(grepl("using .* core\\(s\\)", msgs)))
  expect_true(any(grepl("availableCores=", msgs)))
})
