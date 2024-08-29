library(tidyverse)
library(ggrepel)
library(glue)

location_id <- rcrea::cities(name = c(
  "Delhi", "Mumbai", "Kolkata", "Varanasi",
  "Lucknow", "Amritsar", "Noida",
  "Beijing",
  "Zhejiazhuang",
  "Guangzhou"
))$id
# month_yoy <- "2024-06-01"
weather_file <- "tmp/weather_documentation.RDS"
source <- c("mee", "cpcb")

configs <- crossing(
  location_id=location_id,
  interaction.depth = c(1, 3, 5, 7),
  learning.rate = c(0.001, 0.01, 0.3),
  trees = 20000,
  lag = c(1), #c(1,2,3),
  cv_folds = 2,
  ntrainings = 1,
  training.fraction=0.8
)


get_dewather_from_config <-  function(lag, interaction.depth, learning.rate, trees, cv_folds, location_id, ntrainings, training.fraction) {
  tryCatch({
    creadeweather::deweather(
      location_id = location_id,
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
      lag = lag,
      ntrainings = ntrainings,
      training.fraction = training.fraction
    ) %>%
      mutate(lag = lag,
             interaction.depth = interaction.depth, 
             learning.rate = learning.rate,
             ntrainings=ntrainings,
             training.fraction=training.fraction,
             cv_folds=cv_folds)
  }, error=function(error){
    print(error)
    return(NULL)
  })
}

library(memoise)
# Need to be done only once!
# get_dewather_from_config_memo <- memoise::memoise(get_dewather_from_config)

# Run all configs
deweathered_trend_debug <- mapply(
  get_dewather_from_config_memo,
  lag = configs$lag,
  interaction.depth = configs$interaction.depth,
  learning.rate = configs$learning.rate,
  trees = configs$trees,
  cv_folds = configs$cv_folds,
  location_id=configs$location_id,
  training.fraction=configs$training.fraction,
  ntrainings=configs$ntrainings,
  SIMPLIFY = F
) %>%
  bind_rows()


result_folder <- "scripts/sensitivity_gbm"
dir.create(result_folder)

saveRDS(deweathered_trend_debug, file.path(result_folder, "deweathered_trend_debug.RDS"))

# deweathered_trend_delhi_debug$cv_folds = configs$cv_folds

# Plot results
deweathered_trend_debug %>%
  select(location_id, result, interaction.depth, learning.rate, lag) %>%
  unnest(result) %>%
  filter(variable=="trend") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  facet_grid(location_id ~ glue("ID: {interaction.depth}") + glue("lag: {lag}") + glue("Learning rate: {learning.rate}"))

ggsave(file.path(result_folder, glue("sensitivity_gbm.jpg")),
       width=18, height=12)

as.data.frame((deweathered_trend_debug$performances[[1]][[1]]))
  
deweathered_trend_debug$models[[1]]

# Extract performance
deweathered_trend_debug %>%
  select(location_id, performances, interaction.depth, learning.rate, lag, models) %>%
  rowwise() %>%
  mutate(performances = list(as.data.frame(performances[[1]]))) %>%
  mutate(ntrees_opt=models$ntrees_opt[[1]]) %>%
  tidyr::unnest(performances) %>%
  select(location_id, interaction.depth, learning.rate, lag, rmse_testing, rsquared_testing, ntrees_opt) %>%
  # gather rmse and r2
  tidyr::gather("variable", "value", c(rmse_testing, rsquared_testing, ntrees_opt)) %>%
  ggplot() +
  geom_bar(aes(y=location_id, x=value, fill=glue("ID: {interaction.depth} - lag: {lag} - Learning rate: {learning.rate}")), stat="identity",
           position="dodge") +
    facet_wrap(~variable, scales="free_x") +
  # Reverse fill legend
  guides(fill = guide_legend(reverse=T))
