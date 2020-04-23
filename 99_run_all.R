
pollutants <- c('NO2')
deg <- 0.2
trees <- 600
samples <- 600
lag <- 3


source('00_prepare_input.R')
source('01_get_weather.R')
source('02_prep_training.R')
source('03_train_model_rmweather.R')

data <- prepare_input(pollutants, deg)
data <- get_weather(meas=data,pollutants,deg)
data <- prep_training(meas_weather=data, pollutants, deg)
train_models_rmweather(meas_weather=data, pollutants, deg, trees, samples, lag)