#' Train models across multiple configurations
#'
#' Iterates over a configuration table and trains a model for each row,
#' attaching the configuration to the results.
#'
#' @param data A tibble with columns `location_id` and `meas_weather` (list-column
#'   of measurement-weather data frames).
#' @param configs A tibble where each row defines a training configuration.
#'   Expected columns include: `engine`, `trees`, `weather_vars`, `time_vars`,
#'   `training_end`, `training.fraction`, etc. See [train_models()] for details.
#'
#' @return A tibble with one row per successful training, containing:
#'   - `models`: List of trained model objects
#'   - `performances`: List of performance metrics
#'   - `data`: The prepared training data
#'   - `config`: The configuration used for this training
#'   - Columns from original `data` (e.g., `location_id`, `poll`, `source`)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results <- train_configs(
#'   data = prepared_data,
#'   configs = tibble(
#'     engine = "gbm",
#'     trees = 500,
#'     weather_vars = list(c("ws", "wd", "air_temp_min")),
#'     time_vars = list(c("yday")),
#'     training_end = "2020-12-31",
#'     output = "trend"
#'   )
#' )
#' }
train_configs <- function(data, configs) {

  config_list <- lapply(
    split(configs, seq(nrow(configs))),
    function(config) lapply(as.list(config), unlist)
  )

  lapply(config_list, function(x) {
    result <- do.call(train_models, c(data = list(data), x))
    if (all(is.na(result))) {
      return(NA)
    }
    result %>%
      mutate(config = list(x))
  }) %>%
    do.call(bind_rows, .)
}


#' Train deweathering models for multiple locations
#'
#' Core training function that handles data preparation, time variable creation,
#' and model fitting for one or more locations. Supports multiple training runs
#' (bootstrap) for uncertainty estimation.
#'
#' @section Engines:
#' Currently only `"gbm"` (Gradient Boosting Machine) is supported.
#' The engine dispatches to [train_gbm()].
#'
#' @section Time Variables:
#' Special handling for certain time variables:
#' - `"month"`: Automatically extracted from date (1-12)
#' - `"season"`: Automatically created as Spring/Summer/Autumn/Winter
#'
#' @param data A tibble with columns:
#'   - `location_id`: Unique location identifier
#'   - `meas_weather`: List-column of data frames with `date`, `value`, and weather variables
#' @param engine Character. Training engine to use. Currently only `"gbm"` is supported.
#' @param trees Integer. Maximum number of trees for GBM. Default is 1000.
#' @param samples Integer. Number of samples for prediction (currently unused). Default is 300.
#' @param lag Integer. Number of lag days to include for weather variables.
#'   Creates `{var}_lag1`, `{var}_lag2`, etc. Default is 0.
#' @param weather_vars Character vector of weather variable names to use as predictors.
#' @param time_vars Character vector of time-based predictors. Special values
#'   `"month"` and `"season"` are auto-generated if not present in data.
#' @param training.fraction Numeric. Fraction of pre-cutoff data for training. Default is 0.9
#' @param normalise Logical. Whether to normalize predictors (passed to engine). Default is FALSE.
#' @param detect_breaks Logical. Whether to detect structural breaks (passed to engine). Default is FALSE.
#' @param training_excluded_dates Date vector. Dates to exclude from training set.
#' @param training_end Date or string. Cutoff date separating training from prediction period.
#' @param ntrainings Integer. Number of training runs for bootstrap uncertainty. Default is 1.
#' @param original_seed Integer. Random seed for reproducibility. Default is 42.
#' @param ... Additional arguments passed to the training engine.
#'
#' @return A tibble with one row per location containing:
#'   - `models`: List of trained model objects (length = `ntrainings`)
#'   - `performances`: List of performance metrics
#'   - `data`: The prepared data with predictions
#'   - Original columns from `data` (except `meas_weather`)
#'
#' Returns `NA` if no valid data is available.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- train_models(
#'   data = prepared_data,
#'   engine = "gbm",
#'   trees = 500,
#'   weather_vars = c("ws", "wd", "air_temp_min", "precip"),
#'   time_vars = c("yday", "date_unix"),
#'   training_end = "2020-12-31"
#' )
#' }
train_models <- function(data,
                         engine,
                         trees = 1000,
                         samples = 300,
                         lag = 0,
                         weather_vars = NULL,
                         time_vars = NULL,
                         training.fraction = 0.9,
                         normalise = FALSE,
                         detect_breaks = FALSE,
                         training_excluded_dates = NULL,
                         training_end,
                         ntrainings = 1,
                         original_seed = 42,
                         ...) {

 # Validate input data
  if (!has_valid_data(data)) {
    warning("No measurements available. Returning NA")
    return(NA)
  }

  engine <- match.arg(engine, c("gbm"))

  # Expand weather vars with lag suffixes if needed
  weather_vars_wlag <- expand_weather_vars_with_lag(weather_vars, lag)

  # Add derived time variables (month, season) if requested
  data <- add_derived_time_vars(data, time_vars)

  # Get the appropriate training function
 train_model <- switch(engine, "gbm" = train_gbm)

  # Train with error handling
  train_model_safe <- function(index, location_id, ...) {
    tryCatch({
      message(sprintf("Training model on location %s", location_id))
      res <- train_model(...)
      res$index <- index
      return(res)
    }, error = function(err) {
      warning(sprintf("Failed to train model: %s", err$message))
      return(NA)
    })
  }

  # Bootstrap wrapper for multiple trainings
  train_model_safe_ntimes <- function(index, location_id, ...) {
    set.seed(original_seed)
    lapply(seq_len(ntrainings), function(i) {
      res <- train_model_safe(index, location_id, ...)
      set.seed(runif(1, 1, 1e6))
      return(res)
    })
  }

  # Add index for tracking
  data$index <- zoo::index(data)

  # Run training across all locations
  result <- pbapply::pbmapply(
    train_model_safe_ntimes,
    index = data$index,
    location_id = data$location_id,
    data = data$meas_weather,
    normalise = normalise,
    training_end = training_end,
    weather_vars = list(weather_vars_wlag),
    time_vars = list(time_vars),
    trees = trees,
    detect_breaks = detect_breaks,
    samples = samples,
    training.fraction = training.fraction,
    training_excluded_dates = list(training_excluded_dates),
    USE.NAMES = FALSE,
    SIMPLIFY = FALSE,
    ...
  )

  # Aggregate results
  aggregate_training_results(result, data)
}


# =============================================================================
# Internal helper functions
# =============================================================================

#' Check if data has valid measurements
#' @keywords internal
has_valid_data <- function(data) {
  if (nrow(data) == 0) return(FALSE)
  if (nrow(data$meas_weather[[1]]) == 0) return(FALSE)
  if (all(is.na(data$meas_weather[[1]]))) return(FALSE)
  TRUE
}


#' Expand weather variable names with lag suffixes
#' @keywords internal
expand_weather_vars_with_lag <- function(weather_vars, lag) {
  if (is.null(lag) || lag <= 0) {
    return(weather_vars)
  }

  day_lags <- seq_len(lag)
  lag_vars <- unlist(lapply(weather_vars, function(x) {
    paste(x, day_lags, sep = "_lag")
  }))

  c(weather_vars, lag_vars)
}


#' Add derived time variables (month, season) to data
#' @keywords internal
add_derived_time_vars <- function(data, time_vars) {

  if ("month" %in% time_vars) {
    data <- data %>%
      rowwise() %>%
      mutate(meas_weather = list(
        meas_weather %>% mutate(month = lubridate::month(date))
      ))
  }

  if ("season" %in% time_vars) {
    data <- data %>%
      rowwise() %>%
      mutate(meas_weather = list(
        meas_weather %>%
          mutate(season = forcats::fct_collapse(
            .f = factor(lubridate::month(date)),
            Spring = c("3", "4", "5"),
            Summer = c("6", "7", "8"),
            Autumn = c("9", "10", "11"),
            Winter = c("12", "1", "2")
          ))
      ))
  }

  data
}


#' Aggregate results from multiple training runs
#' @keywords internal
aggregate_training_results <- function(result, data) {

  # Remove failed trainings
  result <- result[!sapply(result, function(x) all(is.na(x)))]

  if (length(result) == 0) {
    return(NA)
  }

  result <- bind_rows(unlist(result, recursive = FALSE))

  if (nrow(result) == 0) {
    return(NA)
  }

  # Combine models per location
  # Note: We keep only the first data to save memory when doing bootstrap
  result <- result %>%
    dplyr::group_by(index) %>%
    dplyr::summarise(
      models = list(model),
      performances = list(performance),
      data = list(dplyr::first(data))
    ) %>%
    dplyr::ungroup() %>%
    # Re-attach location metadata
    dplyr::left_join(
      data %>% dplyr::select(-c(meas_weather)),
      by = "index"
    ) %>%
    dplyr::select(-c(index))

  return(result)
}
