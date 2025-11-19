library(testthat)

skip("Skipping deweather yoy as it requires ERA5 download")

test_that("deweather yoy one month", {
    # Testthat changes working directory
    # -> the embedded load_dot_env doesn't find env file
    
    readRenviron(".Renviron")
    readRenviron("../../.Renviron")
    
    months <- "2024-07-01"
    # weather_file <- tempfile(fileext = ".rds")
    weather_file <- "tmp/weather_yoy.RDS"

    location_id <- rcrea::cities(name=c("Beijing", "Changchun"))$id
    
    deweathered <- creadeweather::deweather_yoy(
        location_id=location_id,
        months = months,
        save_weather_filename = weather_file,
        read_weather_filename = weather_file,
        upload_results = F,
        deweather_process_id = "default_anomaly_2018_2099",
        poll = "pm25"
    )

    # Find
    yoys <- deweathered %>%
      filter(source=="mee") %>%
      tidyr::unnest(result) %>%
      mutate(date=as.character(date))
    
    # All variables should be yoy
    expect_true(all(grepl("yoy", yoys$variable)))
    
    expect_equal(nrow(yoys), length(months) * length(location_id) * 6)
    
    
    # Based on previously issued snapshot
    # https://energyandcleanair.org/china-energy-and-emissions-trends-august-2024-snapshot/
    expected <-
      list(
        list(
          date = "2024-07-01",
          location_id="beijing_chn.2_1_cn",
          variable=c("yoy_total_rel", "yoy_weather_rel"),
          value_expected=c(0.4, 0.3)
        ),
        list(
          date = "2024-07-01",
          location_id="changchun_chn.17_1_cn",
          variable=c("yoy_total_rel", "yoy_weather_rel"),
          value_expected=c(1, -0.2)
        )
      ) %>% tibble(data=.) %>%
      unnest_wider(data) %>%
      unnest(cols = c(variable, value_expected))
    
    # Create tibble and join
    comparison <- expected %>%
      left_join(yoys, by=c("date", "location_id", "variable"))

    
    expect_equal(nrow(comparison), length(months) * length(location_id) * 2)

    expect_true(
      all(comparison %>%
            mutate(diff=abs(value - value_expected)) %>%
            pull(diff) < 0.1)
    )
})
