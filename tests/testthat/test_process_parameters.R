test_that("process_id_to_parameters selects and parses process parameters", {
  process_rows <- tibble::tibble(
    id = c("other-process", "target-process"),
    parameters = c(
      "{\"ignored\":true}",
      paste0(
        "{\"window\":7,\"enabled\":true,",
        "\"pollutants\":[\"pm25\",\"no2\"],",
        "\"nested\":{\"threshold\":12.5},\"optional\":null}"
      )
    )
  )

  testthat::local_mocked_bindings(
    processes = function() process_rows,
    .package = "rcrea"
  )

  expect_identical(
    process_id_to_parameters("target-process", "parameters"),
    list(
      window = 7,
      enabled = TRUE,
      pollutants = c("pm25", "no2"),
      nested = list(threshold = 12.5),
      optional = NULL
    )
  )
})
