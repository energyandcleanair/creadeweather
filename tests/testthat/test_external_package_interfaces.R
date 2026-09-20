test_that("deweather provides the time variables used by train_caret", {
  input <- data.frame(
    date = as.POSIXct(
      c("2024-01-01 00:00:00", "2024-07-15 13:00:00"),
      tz = "UTC"
    ),
    value = c(10, 20)
  )

  result <- deweather::prepData(
    input,
    add = c("hour", "weekday", "trend", "week", "jday", "month")
  )

  expect_equal(result$date, input$date)
  expect_equal(result$value, input$value)
  expect_equal(result$hour, c(0, 13))
  expect_equal(as.character(result$weekday), c("Monday", "Monday"))
  expect_equal(result$trend, as.numeric(input$date))
  expect_equal(result$week, c(1, 29))
  expect_equal(result$jday, c(1, 197))
  expect_equal(as.character(result$month), c("Jan", "Jul"))
})

test_that("ggpubr renders the model information table", {
  output <- list(
    location_id = "station-1",
    gadm1_id = "region-1",
    gadm1_name = "Region One",
    pollutant = "pm25",
    model_name = "gbm",
    rsq = 0.91,
    rsq_test = 0.82,
    mae = 1.1,
    mae_test = 1.2,
    mase = 1.3,
    mase_test = 1.4,
    mrae = 1.5,
    mrae_test = 1.6,
    me = 1.7,
    me_test = 1.8,
    mpe = 1.9,
    mpe_test = 2.0
  )

  table_plot <- creadeweather:::plot.infos(output)

  expect_s3_class(table_plot, "ggplot")
  expect_true(is.function(ggpubr::ggarrange))
  expect_true(is.function(ggpubr::annotate_figure))
  expect_true(is.function(ggpubr::ggexport))
})
