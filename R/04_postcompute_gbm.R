#' Post-process GBM training results
#'
#' Transforms raw GBM training outputs into tidy results with predictions,
#' anomalies, trends, and fire counterfactuals. Handles multiple bootstrap
#' models by averaging predictions.
#'
#' @section Output Variables:
#' - `observed`: Original pollution values (inverse-transformed if log link)
#' - `predicted`: Model predictions (averaged across bootstrap models)
#' - `anomaly`: Difference between observed and predicted
#' - `trend`: Time trend from partial dependence (if time_vars specified)
#' - `predicted_nofire`: Counterfactual without fire effect (if add_fire=TRUE)
#'
#' @param models List of trained GBM model objects.
#' @param data Data frame with predictors and response variable.
#' @param config List containing training configuration:
#'   - `weather_vars`: Weather variable names
#'   - `time_vars`: Time variable names
#'   - `add_fire`: Whether to compute fire counterfactual
#'   - `link`: Link function ("linear" or "log")
#' @param performances List of performance metrics from training.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with columns:
#'   - `config`: The configuration used
#'   - `models`: Lightened model summary (importance, optimal trees)
#'   - `result`: Long-format results with date, variable, value
#'   - `performances`: Performance metrics
#'
#' @keywords internal
postcompute_gbm <- function(models, data, config, performances, ...) {

  weather_vars <- config$weather_vars
  time_vars <- config$time_vars
  add_fire <- config$add_fire

  # Setup link functions
  if (config$link == "linear") {
    do_link <- identity
    do_unlink <- identity
  } else if (config$link == "log") {
    do_link <- log
    do_unlink <- exp
  } else {
    stop("Link function must be 'linear' or 'log'")
  }

  # Generate predictions (average across bootstrap models)
  data$index <- zoo::index(data)

  predicted <- lapply(models, function(model) {
    tibble(
      index = data$index,
      predicted = do_unlink(predict(model, data, n.trees = model$n.trees.opt))
    )
  }) %>%
    bind_rows() %>%
    dplyr::group_by(index) %>%
    dplyr::summarise(
      predicted = mean(predicted),
      predicted_p975 = quantile(predicted, 0.975),
      predicted_p025 = quantile(predicted, 0.025)
    )

  data <- data %>%
    dplyr::select(-c(predicted)) %>%
    dplyr::left_join(predicted, by = "index")

  # Inverse-transform response and compute anomaly
  data$value <- do_unlink(data$value)
  data$observed <- data$value
  data$anomaly <- data$observed - data$predicted

  # Compute fire counterfactual if requested
  if (add_fire) {
    data <- postcompute_gbm_fire(
      data = data,
      models = models,
      do_unlink = do_unlink,
      weather_vars = weather_vars
    )
  }

  # Extract time trend partial dependencies
  if (length(time_vars) > 0) {
    data <- postcompute_gbm_trends(
      data = data,
      time_vars = time_vars,
      models = models,
      do_unlink = do_unlink
    )
  }

  # Select output columns
  output_cols <- c("date", "anomaly", "predicted", "observed", "trend")
  output_cols <- c(output_cols, names(data)[grepl("trend_", names(data))])

  if (add_fire) {
    output_cols <- c(output_cols, names(data)[grepl("predicted_nofire", names(data))])
  }

  # Reshape to long format
  result <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(output_cols)) %>%
    dplyr::mutate(date = lubridate::date(date)) %>%
    dplyr::arrange(date) %>%
    tidyr::gather("variable", "value", -c(date)) %>%
    suppressWarnings()

  # Create lightweight model summary
  models_light <- postcompute_gbm_lighten_model(models = models, data = data)

  tibble(
    config = list(config),
    models = list(models_light),
    result = list(result),
    performances = list(performances)
  )
}


#' Create lightweight model summary for storage
#'
#' Extracts only essential information from GBM models to reduce storage size.
#' Full models can be several MB each; this reduces to a few KB.
#'
#' @param models List of trained GBM model objects.
#' @param data Data frame (unused, kept for API consistency).
#'
#' @return List containing:
#'   - `importance`: Variable importance from each model
#'   - `ntrees_opt`: Optimal tree count for each model
#'
#' @keywords internal
postcompute_gbm_lighten_model <- function(models, data) {

  importance <- lapply(models, function(m) summary(m, plot_it = FALSE))
  ntrees_opt <- lapply(models, function(m) m$n.trees.opt)

  list(
    importance = importance,
    ntrees_opt = ntrees_opt
  )
}


#' Compute fire counterfactual predictions
#'
#' Creates predictions for a counterfactual scenario where fire variables
#' are set to zero. This allows attributing pollution to fire vs other factors.
#'
#' @section Fire Variable Detection:
#' Uses [fire.extract_vars_by_region()] to identify fire variables and group
#' them by region suffix (e.g., `fire_frp`, `fire_frp_IDN`, `fire_frp_MYS`).
#'
#' @param data Data frame with predictions and fire variables.
#' @param models List of trained GBM model objects.
#' @param do_unlink Inverse link function.
#' @param weather_vars Weather variable names (unused, kept for API consistency).
#'
#' @return Data frame with additional `predicted_nofire*` columns.
#'
#' @keywords internal
postcompute_gbm_fire <- function(data, models, do_unlink, weather_vars) {

  # Get variable names from model
  formula_vars <- models[[1]]$variables$var_names
  stopifnot("Could not extract variable names from model" = !is.null(formula_vars))

  fire_groups <- fire.extract_vars_by_region(formula_vars)

  # Create counterfactual for each fire region group
  for (i in seq_len(nrow(fire_groups))) {
    suffix <- fire_groups$suffix[i]
    fire_vars_group <- fire_groups$vars[[i]]

    data_nofire <- data
    data_nofire[, fire_vars_group] <- 0

    # Name: predicted_nofire or predicted_nofire_XXX
    predicted_name <- if (suffix == "") {
      "predicted_nofire"
    } else {
      paste0("predicted_nofire", suffix)
    }

    data[, predicted_name] <- sapply(
      models,
      function(model) do_unlink(predict(model, data_nofire, n.trees = model$n.trees.opt))
    ) %>%
      rowMeans(na.rm = TRUE)
  }

  # Create overall no-fire counterfactual (all fire vars = 0)
  all_fire_vars <- unlist(fire_groups$vars, use.names = FALSE)

  if (length(all_fire_vars) > 0) {
    data_nofire <- data
    data_nofire[, all_fire_vars] <- 0

    data[, "predicted_nofire"] <- sapply(
      models,
      function(model) do_unlink(predict(model, data_nofire, n.trees = model$n.trees.opt))
    ) %>%
      rowMeans(na.rm = TRUE)
  } else if (!("predicted_nofire" %in% names(data))) {
    # No fire variables found but add_fire=TRUE: use predicted as counterfactual
    data[, "predicted_nofire"] <- data$predicted
  }

  return(data)
}


#' Extract time trend from partial dependence
#'
#' Computes the partial dependence of the model on time variables to extract
#' the underlying trend. Currently supports `date_unix` as the time variable.
#'
#' @param data Data frame with date column.
#' @param time_vars Character vector of time variable names.
#' @param models List of trained GBM model objects.
#' @param do_unlink Inverse link function.
#'
#' @return Data frame with additional `trend` column.
#'
#' @keywords internal
postcompute_gbm_trends <- function(data, time_vars, models, do_unlink) {

  dates <- data %>%
    dplyr::distinct(date) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      yday_joiner = lubridate::yday(date),
      month_joiner = lubridate::month(date),
      wday_joiner = lubridate::wday(date, week_start = 1),
      season_joiner = forcats::fct_collapse(
        .f = factor(lubridate::month(date)),
        Spring = c("3", "4", "5"),
        Summer = c("6", "7", "8"),
        Autumn = c("9", "10", "11"),
        Winter = c("12", "1", "2")
      )
    )

  # Extract trend from date_unix partial dependence
  if ("date_unix" %in% time_vars) {
    ndays <- as.numeric(max(dates$date) - min(dates$date), units = "days")

    trend_data <- lapply(models, function(model) {
      plot(model, "date_unix", continuous_resolution = ndays, return_grid = TRUE)
    }) %>%
      bind_rows() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(y = do_unlink(y)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(date = lubridate::date(lubridate::date_decimal(date_unix))) %>%
      dplyr::select(date, date_unix = y) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(
        trend = mean(date_unix, na.rm = TRUE),
        trend_p975 = quantile(date_unix, 0.975, na.rm = TRUE),
        trend_p025 = quantile(date_unix, 0.025, na.rm = TRUE),
        .groups = "drop"
      )

    dates <- dates %>% dplyr::left_join(trend_data, by = "date")
  }

  # Select trend columns and join back to data
  trend_vars <- dates %>%
    dplyr::select(date, dplyr::starts_with("trend"))

  data %>%
    dplyr::full_join(trend_vars, by = "date")
}


#' Extract weather-normalized trend (EXPERIMENTAL)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Alternative method to extract trend by averaging predictions over sampled

#' weather conditions. This approach normalizes out weather variation but is
#' computationally expensive.
#'
#' @param data Data frame with date and weather columns.
#' @param time_vars Character vector of time variable names.
#' @param model Single trained GBM model object.
#' @param do_unlink Inverse link function.
#'
#' @return A ggplot object showing the trend (for visual inspection).
#'
#' @keywords internal
postcompute_gbm_trends_w_weather <- function(data, time_vars, model, do_unlink) {

  size_sample <- 5000
  weather_sample <- data %>% dplyr::sample_n(size_sample)

  get_normalised_trend_at_date <- function(target_date) {
    # Get time vars for target date
    time_vars_df <- data %>%
      dplyr::filter(date == !!target_date) %>%
      dplyr::select(dplyr::any_of("date_unix"))

    # Apply to weather sample
    date_sample <- weather_sample %>%
      dplyr::mutate(!!!time_vars_df)

    # Predict and average
    predicted <- do_unlink(predict(model, date_sample))
    mean(predicted, na.rm = TRUE)
  }

  # Compute trend for each unique date
  unique_dates <- unique(data$date)
  trends <- pbapply::pbsapply(unique_dates, get_normalised_trend_at_date)

  # Return plot for visual inspection
  tibble::tibble(date = unique_dates, trend = unlist(trends)) %>%
    dplyr::arrange(date) %>%
    ggplot2::ggplot(ggplot2::aes(date, trend)) +
    ggplot2::geom_line()
}
