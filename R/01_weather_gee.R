
gee.get_weather <- function() {
  
  
  #-------------------------
  # Preparing weather data
  #-------------------------
  csv_pattern <- Sys.glob("data/01_weather/output/glas_gadm1/*.csv")
  csv_files <- lapply(csv_pattern, read.csv)
  
  csv_tbls <- lapply(csv_files, function(x) {
    tryCatch({
      new_tibble(x) %>% 
        dplyr::select(
          id, GID_1, NAME_1, Psurf_f_inst, Qair_f_inst, Rainf_tavg, Tair_f_inst, Wind_f_inst
        )
    }, error = function(cond) {
      print(cond)
      NA
    })
  })
  
  print(paste("Wrong tibbles: [", length(csv_tbls[is.na(csv_tbls)]), "/", length(csv_tbls), "]"))
  csv_tbls <- csv_tbls[!is.na(csv_tbls)]
  
  weath_3h_gadm <- do.call(bind_rows, csv_tbls)
  weath_3h_gadm <- weath_3h_gadm %>% 
    mutate(date = as.POSIXct(id, format = "A%Y%m%d_%H%M", tz = "UTC"))
  
  # Average per day
  weath_1d_gadm <- weath_3h_gadm %>% 
    mutate(date = lubridate::date(date)) %>%
    group_by(GID_1, NAME_1, date) %>%
    summarise(
      Psurf_f_inst = mean(Psurf_f_inst, na.rm = TRUE),
      Qair_f_inst = mean(Qair_f_inst, na.rm = TRUE),
      Rainf_tavg = mean(Psurf_f_inst, na.rm = TRUE),
      Tair_f_inst = mean(Tair_f_inst, na.rm = TRUE),
      Wind_f_inst = mean(Wind_f_inst, na.rm = TRUE)
    ) %>%
    rename(gadm1_id = GID_1, gadm1_name = NAME_1) %>% 
    group_by(gadm1_id, gadm1_name) %>%
    tidyr::nest() %>%
    rename(weather = data)
  
  
  result_path <- file.path("data", "01_weather", "output", "gadm1_weather_gee.RDS")
  saveRDS(weath_1d_gadm, result_path)
}