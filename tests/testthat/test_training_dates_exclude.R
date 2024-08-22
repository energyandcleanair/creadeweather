library(testthat)



test_that("attaching fire", {
    # Testthat changes working directory
    # -> the embedded load_dot_env doesn't find env file
    library(tidyverse)
    library(facetscales)
    library(ggrepel)

    # create temp file for weather to speedup process
    weather_file <- tempfile(fileext = ".rds")

    # To make it quicker, we only do one year
    date_from <- "2020-01-01"
    date_to <- "2020-12-31"
    dates <- as.Date(seq(as.Date(date_from), as.Date(date_to), by = "day"))
    location_id <- rcrea::cities(name = "Delhi")$id
    meas <- rcrea::measurements(location_id = location_id, date_from = date_from, date_to = date_to, poll = "pm25", with_geometry = T)
    
    deweathered1 <- creadeweather::deweather(
        location_id = location_id,
        meas = meas,
        deweather_process_id = "default_anomaly_2018_2099",
        poll = "pm25",
        upload_results = F,
        save_weather_filename = weather_file
    )
    
    deweathered1bis <- creadeweather::deweather(
      location_id = location_id,
      meas = meas,
      deweather_process_id = "default_anomaly_2018_2099",
      poll = "pm25",
      upload_results = F,
      read_weather_filename = weather_file
    )
    
    
    # Assert they're equal (we fix seed in gbm)
    expect_equal(
      deweathered1$model[[1]]$rmse_training,
      deweathered1bis$model[[1]]$rmse_training
    )
    
    #Now exclude 10% of dates -> it should succeed, but have different rmse
    training_excluded_dates <- sample(dates, as.integer(length(dates) * 0.1))
    
    deweathered2 <- creadeweather::deweather(
      location_id = location_id,
      meas = meas,
      deweather_process_id = "default_anomaly_2018_2099",
      poll = "pm25",
      upload_results = F,
      read_weather_filename = weather_file,
      training_excluded_dates = training_excluded_dates
      )
    
    expect_true(
      deweathered1$model[[1]]$rmse_training !=
      deweathered2$model[[1]]$rmse_training
    )
    
    
    #Now exclude 95% of dates -> it should fail
    dates <- as.Date(seq(as.Date(date_from), as.Date(date_to), by = "day"))
    training_excluded_dates <- sample(dates, as.integer(length(dates) * 0.95))

    # Assert that it fails
    testthat::expect_error(
      deweathered2 <- creadeweather::deweather(
        location_id = location_id,
        meas = meas,
        deweather_process_id = "default_anomaly_2018_2099",
        poll = "pm25",
        upload_results = F,
        training_excluded_dates = training_excluded_dates,
        read_weather_filename = weather_file
    ))
    
    # Remove file
    unlink(weather_file)
})
