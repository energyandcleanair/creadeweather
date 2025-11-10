# Helper providing synthetic_train_inputs() for integration tests
synthetic_train_inputs <- function(
  seed = 123,
  training_days = 335,
  prediction_days = 30,
  # TREND PARAMETERS
  include_trend = TRUE,
  trend_drop = 5,
  # ANOMALY PARAMETERS
  include_anomaly = TRUE,
  anomaly_start_day = training_days + 1,
  anomaly_length = 7,
  anomaly_magnitude = 12,
  # FIRE PARAMETERS
  include_fire = FALSE,
  fire_start_day = training_days + 1,
  fire_length = 10,
  fire_random = FALSE,
  fire_prediction_frac = 0.4,
  fire_magnitude = 5,
  fire_pollution_effect = 2,
  noise_sd = 0.5
) {
  set.seed(seed)

  total_days <- training_days + prediction_days
  start_date <- as.Date("2022-01-01")
  dates <- seq(start_date, by = "day", length.out = total_days)
  day_index <- seq_len(total_days)

  temp <- 12 + sin(2 * pi * day_index / 30) * 8
  humidity <- 60 + cos(2 * pi * day_index / 15) * 10
  wind_speed <- 2 + sin(2 * pi * day_index / 7)
  wind_direction <- (day_index * 20) %% 360
  precip <- ifelse(day_index %% 10 == 0, 5, 0)
  atmos_pres <- 1013 + sin(2 * pi * day_index / 28) * 3
  dewpoint <- temp - 4 + stats::rnorm(total_days, 0, 0.2)
  pbl_min <- 180 + sin(2 * pi * day_index / 20) * 25
  pbl_max <- pbl_min + 60

  base_signal <- 30 +
    0.4 * temp -
    0.25 * humidity +
    0.8 * wind_speed -
    0.03 * precip +
    0.01 * pbl_min

  trend_component <- if (include_trend) {
    seq(0, -trend_drop, length.out = total_days)
  } else {
    rep(0, total_days)
  }

  value <- base_signal + trend_component + stats::rnorm(total_days, mean = 0, sd = noise_sd)

  anomaly_idx <- integer(0)
  if (include_anomaly) {
    anomaly_idx <- seq(anomaly_start_day, length.out = anomaly_length)
    anomaly_idx <- anomaly_idx[anomaly_idx <= total_days]
    value[anomaly_idx] <- value[anomaly_idx] + anomaly_magnitude
  }

  fire_indicator <- rep(0, total_days)
  fire_frp <- rep(0, total_days)
  fire_idx <- integer(0)
  fire_contrib <- rep(0, total_days)
  if (include_fire) {
    fire_idx <- if (fire_random) {
      sort(sample(seq_len(total_days), size = min(fire_length, total_days), replace = FALSE))
    } else {
      idx <- seq(fire_start_day, length.out = fire_length)
      idx[idx <= total_days]
    }

    if (length(fire_idx) == 0) {
      fire_idx <- integer(0)
    }

    fire_indicator[fire_idx] <- 1
    fire_frp[fire_idx] <- fire_magnitude + stats::rnorm(length(fire_idx), mean = 0, sd = fire_magnitude * 0.1)
    fire_frp <- pmax(fire_frp, 0)
    fire_contrib <- fire_pollution_effect * fire_frp
    value <- value + fire_contrib
    if (length(fire_idx) > 0) {
      value[fire_idx] <- value[fire_idx] + stats::rnorm(length(fire_idx), mean = 0, sd = fire_pollution_effect * 0.1)
    }
  }

  weather_vars <- c(
    "wd",
    "ws",
    "precip",
    "atmos_pres",
    "air_temp_min",
    "air_temp_max",
    "dewpoint_temp",
    "pbl_min",
    "pbl_max"
  )
  if (include_fire) {
    weather_vars <- c(weather_vars, "fire_frp")
  }

  meas_weather <- tibble::tibble(
    date = as.POSIXct(dates),
    timezone = "UTC",
    value = value,
    wd = wind_direction,
    ws = wind_speed,
    precip = precip,
    atmos_pres = atmos_pres,
    air_temp_min = temp - 2,
    air_temp_max = temp + 2,
    dewpoint_temp = dewpoint,
    pbl_min = pbl_min,
    pbl_max = pbl_max,
    fire_frp = fire_frp
  ) %>%
    dplyr::mutate(
      wd_factor = factor(floor(wd / 45)),
      yday = lubridate::yday(date),
      date_unix = lubridate::decimal_date(date)
    )

  prepared_data <- tibble::tibble(
    location_id = 1L,
    poll = "pm25",
    unit = "ug/m3",
    source = "synthetic",
    process_id = "synthetic_train_test",
    meas_weather = list(meas_weather)
  )

  anomaly_training_end <- as.character(dates[training_days])
  if (include_fire) {
    anomaly_training_end <- as.character(dates[total_days])
  }
  trend_training_end <- as.character(dates[total_days])

  outputs <- c(
    if (include_anomaly) "anomaly",
    if (include_trend) "trend"
  )

  if (include_fire && !"anomaly" %in% outputs) {
    outputs <- c(outputs, "anomaly")
  }

  if (length(outputs) == 0) {
    stop("No outputs requested; enable trend and/or anomaly.")
  }

  configs <- purrr::map(outputs, function(output) {
    time_vars <- if (output == "anomaly") c("yday") else c("date_unix")
    training_end <- if (output == "anomaly") anomaly_training_end else trend_training_end

    tibble::tibble(
      engine = "gbm",
      trees = 200,
      samples = 300,
      lag = 0,
      training.fraction = 0.8,
      weather_vars = list(weather_vars),
      time_vars = list(time_vars),
      normalise = FALSE,
      detect_breaks = FALSE,
      training_end = training_end,
      output = output,
      link = "linear",
      learning.rate = 0.1,
      interaction.depth = 2,
      cv_folds = 3,
      keep_model = FALSE,
      training_start = as.character(dates[1]),
      add_fire = include_fire
    )
  }) %>%
    dplyr::bind_rows()

  fire_prediction_idx <- fire_idx
  fire_prediction_dates <- as.Date(dates[fire_prediction_idx])

  expectations <- list(
    include_trend = include_trend,
    include_anomaly = include_anomaly,
    include_fire = include_fire,
    anomaly_window = as.Date(dates[anomaly_idx]),
    anomaly_training_end = as.Date(anomaly_training_end),
    anomaly_magnitude = if (include_anomaly) anomaly_magnitude else 0,
    trend_drop = if (include_trend) trend_drop else 0,
    fire_window = fire_prediction_dates,
    fire_pollution_effect = if (include_fire) fire_pollution_effect else 0,
    fire_days = length(fire_idx),
    fire_average_contribution = mean(fire_contrib),
    fire_expected_delta = if (length(fire_prediction_idx) > 0) mean(fire_contrib[fire_prediction_idx]) else 0
  )

  list(
    data = prepared_data,
    configs = configs,
    expectations = expectations
  )
}
