library(testthat)

skip("Skipping deweathering as it is more of a result validation")
test_that("deweathering", {
  
  # Testthat changes working directory
  # -> the embedded load_dot_env doesn't find env file
  env_file <- "../../.env"
  if (file.exists(env_file)) {
    dotenv::load_dot_env(env_file)
  }
  
  # A station in Brussels
  location_id <- rcrea::cities(name="delhi")$id
  source <- "cpcb"
  poll <- rcrea::NO2
  
  results <- creadeweather::deweather(
    poll=poll,
    source=source,
    location_id=location_id,
    output=c("anomaly","trend"),
    upload_results=F
  )
  
  expect_equal(nrow(results), 2)
  toyday <- lubridate::yday(lubridate::today())
  expect_gte(sum(complete.cases(results$normalised[[1]])), 3*365 + toyday - 10) # Trend
  expect_gte(sum(complete.cases(results$normalised[[2]])), toyday) # Anomaly
  
})

