library(testthat)

test_that("deweathering", {
  
  # Testthat changes working directory
  # -> the embedded load_dot_env doesn't find env file
  dotenv::load_dot_env("../../.env")
  
  # A station in Brussels
  location_id <- "sta-betn043"
  source <- "eea"
  polls <- rcrea::NO2
  
  results <- deweather(
    polls=polls,
    source=source,
    location_id=location_id,
    aggregate_level="station",
    output=c("anomaly","trend"),
    upload_results=F
  )
  
  expect_equal(nrow(results), 2)
  toyday <- lubridate::yday(lubridate::today())
  expect_gte(sum(complete.cases(results$normalised[[1]])), 3*365 + toyday - 10) # Trend
  expect_gte(sum(complete.cases(results$normalised[[2]])), toyday) # Anomaly
  
})