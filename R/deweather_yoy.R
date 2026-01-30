#' Compute year-over-year changes and attribute to weather vs emissions
#'
#' Runs deweathering while excluding specific months from training to compute
#' unbiased year-over-year (YoY) changes. Attributes total change to weather
#' effects vs emission changes.
#'
#' @section Use Case:
#' This function is used by CREA's monthly snapshot products to track pollution
#' trends and attribute changes to weather patterns vs actual emission reductions.
#'
#' @section Methodology:
#' For each month, the function:
#' 1. Excludes the target month and the same month from the previous year from training
#' 2. Trains a deweathering model on remaining data
#' 3. Computes predicted (weather-driven) and observed values for both months
#' 4. Calculates YoY changes and attributes them to weather vs emissions
#'
#' @param months Character vector of months to analyze (format: "YYYY-MM-01").
#' @param upload_results Logical. Whether to upload results to the database.
#' @param deweather_process_id Character. Process ID for deweathering configuration.
#' @param keep_nonyoy_results Logical. Whether to keep original results alongside
#'   YoY results. Default is FALSE.
#' @param save_weather_filename Optional path to save weather data for reuse.
#' @param read_weather_filename Optional path to read cached weather data.
#' @param ... Additional arguments passed to [deweather()].
#'
#' @return A tibble with YoY results for each month, containing variables:
#'   - `yoy_total`: Absolute change in observed pollution
#'   - `yoy_weather`: Weather-driven change (from model predictions)
#'   - `yoy_emission`: Emission-driven change (total - weather)
#'   - `yoy_total_rel`, `yoy_weather_rel`, `yoy_emission_rel`: Relative changes
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze July 2024 for Beijing
#' yoy_results <- deweather_yoy(
#'   months = "2024-07-01",
#'   location_id = "beijing_chn.2_1_cn",
#'   poll = "pm25",
#'   deweather_process_id = "default_anomaly_2018_2099",
#'   upload_results = FALSE
#' )
#' }
deweather_yoy <- function(months,
                          upload_results,
                          deweather_process_id,
                          keep_nonyoy_results = FALSE,
                          save_weather_filename = NULL,
                          read_weather_filename = NULL,
                          ...) {

  # Setup weather file caching
  keep_weather_file <- !is.null(save_weather_filename)

  if (is.null(save_weather_filename)) {
    save_weather_filename <- tempfile(fileext = ".RDS")
  }

  if (is.null(read_weather_filename)) {
    read_weather_filename <- save_weather_filename
  }

  # Process each month
 deweathered_yoys <- lapply(months, function(month) {
    # Exclude target month and same month from previous year
    excluded_dates <- get_excluded_yoy_dates(month)

    deweathered <- deweather(
      ...,
      deweather_process_id = deweather_process_id,
      upload_results = FALSE,  # Upload after YoY extraction if needed
      training_excluded_dates = excluded_dates,
      save_weather_filename = save_weather_filename,
      read_weather_filename = read_weather_filename
    )

    deweathered_yoy <- extract_yoy_changes(deweathered, month, keep_nonyoy_results)

    if (upload_results) {
      upload_results(
        results = deweathered_yoy,
        deweather_process_id = deweather_process_id
      )
    }

    return(deweathered_yoy)
  }) %>%
    bind_rows()

  # Cleanup temporary weather file
  if (!keep_weather_file) {
    file.remove(save_weather_filename)
  }

  return(deweathered_yoys)
}


#' Extract YoY changes from deweathered results
#'
#' Applies [extract_yoy_changes_from_result()] to each result in the
#' deweathered data structure.
#'
#' @param deweathered Tibble with `result` list-column from [deweather()].
#' @param month Target month (format: "YYYY-MM-01").
#' @param keep_nonyoy_results Whether to keep original results.
#'
#' @return Modified deweathered tibble with YoY results.
#'
#' @export
extract_yoy_changes <- function(deweathered, month, keep_nonyoy_results) {

  new_results <- lapply(deweathered$result, function(result) {
    yoy_result <- tryCatch(
      extract_yoy_changes_from_result(result, month),
      error = function(e) NULL
    )

    if (keep_nonyoy_results && !is.null(result)) {
      bind_rows(result, yoy_result)
    } else {
      yoy_result
    }
  })

  deweathered$result <- new_results
  return(deweathered)
}


#' Calculate YoY changes from a result dataframe
#'
#' Computes year-over-year changes in pollution and attributes them to
#' weather effects vs emission changes.
#'
#' @section Output Variables:
#' - `yoy_total`: Total observed change (this year - last year)
#' - `yoy_weather`: Weather-driven change (predicted this year - predicted last year)
#' - `yoy_emission`: Emission change (yoy_total - yoy_weather)
#' - `*_rel`: Relative changes (divided by previous year's observed value)
#'
#' @param result Dataframe with columns: `date`, `variable`, `value`.
#'   Must contain "observed" and "predicted" variables.
#' @param month Target month (format: "YYYY-MM-01").
#'
#' @return Dataframe with columns: `date`, `variable`, `value` containing
#'   the 6 YoY metrics.
#'
#' @export
#'
#' @examples
#' \dontrun
#' # Create synthetic result data
#' result <- data.frame(
#'   date = rep(c(as.Date("2023-07-01"), as.Date("2024-07-01")), each = 2),
#'   variable = rep(c("observed", "predicted"), 2),
#'   value = c(50, 45, 55, 48)  # Last year: obs=50, pred=45; This year: obs=55, pred=48
#' )
#' yoy <- extract_yoy_changes_from_result(result, "2024-07-01")
#' # yoy_total = 5, yoy_weather = 3, yoy_emission = 2
#' }
extract_yoy_changes_from_result <- function(result, month) {

  if (is.null(result)) return(NULL)

  excluded_dates <- get_excluded_yoy_dates(month)

  # Filter to relevant dates and variables
  filtered <- result %>%
    dplyr::filter(
      date %in% excluded_dates,
      variable %in% c("observed", "predicted")
    )

  if (nrow(filtered) == 0) return(NULL)

  # Calculate yearly averages
  yearly_means <- filtered %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::group_by(variable, year) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(year)

  # Need exactly 2 years of data
  years <- unique(yearly_means$year)
  if (length(years) != 2) return(NULL)

  # Get baseline (previous year's observed value) for relative calculations
  observed_prev <- yearly_means %>%
    dplyr::filter(year == min(year), variable == "observed") %>%
    dplyr::pull(value)

  if (length(observed_prev) == 0 || is.na(observed_prev)) return(NULL)

  # Calculate year-over-year deltas
  yoy_deltas <- yearly_means %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(delta = value - dplyr::lag(value)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(delta)) %>%
    dplyr::select(variable, delta) %>%
    tidyr::pivot_wider(names_from = variable, values_from = delta, names_prefix = "yoy_")

  if (nrow(yoy_deltas) == 0) return(NULL)

  # Rename and calculate derived metrics
  yoy_metrics <- yoy_deltas %>%
    dplyr::rename(
      yoy_total = yoy_observed,
      yoy_weather = yoy_predicted
    ) %>%
    dplyr::mutate(
      yoy_emission = yoy_total - yoy_weather,
      yoy_total_rel = yoy_total / observed_prev,
      yoy_weather_rel = yoy_weather / observed_prev,
      yoy_emission_rel = yoy_emission / observed_prev
    )

  # Reshape to long format
  yoy_metrics %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(date = as.Date(month)) %>%
    dplyr::select(date, variable, value)
}


#' Get dates to exclude from training for YoY analysis
#'
#' Returns all days in the target month and the same month from the previous
#' year. These dates are excluded from model training to ensure unbiased
#' YoY comparisons.
#'
#' @param month Target month (format: "YYYY-MM-01" or any date in the month).
#'
#' @return Date vector containing all days in both months.
#'
#' @export
#'
#' @examples
#' dates <- get_excluded_yoy_dates("2024-07-01")
#' # Returns all days in July 2023 and July 2024
get_excluded_yoy_dates <- function(month) {
  month_current <- as.Date(month)
  month_previous <- month_current - lubridate::years(1)

  # Get all days in both months
  days_current <- seq.Date(
    from = month_current,
    to = month_current + lubridate::days_in_month(month_current) - 1,
    by = "day"
  )

  days_previous <- seq.Date(
    from = month_previous,
    to = month_previous + lubridate::days_in_month(month_previous) - 1,
    by = "day"
  )

  c(days_previous, days_current)
}
