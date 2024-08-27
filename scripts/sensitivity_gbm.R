library(tidyverse)
library(ggrepel)
library(glue)

location_id <- rcrea::cities(name = c(
  "Delhi", "Mumbai", "Kolkata", "Varanasi",
  "Lucknow", "Amritsar", "Noida"
))$id
month_yoy <- "2024-06-01"
weather_file <- "tmp/weather_documentation.RDS"


configs <- crossing(
  interaction.depth = c(1, 3, 5),
  learning.rate = c(0.01, 0.1, 0.3),
  trees = 20000,
  lag = c(1,2,3),
  cv_folds = c(2,4)
)

# Run all configs

deweathered_trend_delhi_debug <- mapply(
  function(lag, interaction.depth, learning.rate, trees, cv_folds) {
    creadeweather::deweather(
      location_id = "delhi_ind.25_1_in",
      poll = "pm25",
      source = "cpcb",
      output="trend",
      upload_results = F,
      read_weather_filename = weather_file,
      weather_update_era5 = F,
      use_weather_cache = T,
      interaction.depth = interaction.depth,
      learning.rate = learning.rate,
      trees = trees,
      cv_folds = cv_folds,
      lag = lag
    ) %>%
      mutate(lag = lag, interaction.depth = interaction.depth, learning.rate = learning.rate)
  },
  lag = configs$lag,
  interaction.depth = configs$interaction.depth,
  learning.rate = configs$learning.rate,
  trees = configs$trees,
  cv_folds = configs$cv_folds
)
