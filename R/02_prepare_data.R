#' Prepare measurement-weather data for model training
#'
#' Applies a complete data preparation pipeline to measurement-weather data,
#' including cleaning, filling missing values, feature enrichment, and filtering.
#' Each location's `meas_weather` tibble is processed independently.
#'
#' The pipeline consists of four steps:
#' 1. **Cleaning** ([clean_data()]): Remove infinite values and outliers (MAD filter)
#' 2. **Filling** ([fill_data()]): Handle missing values via zero-fill or interpolation
#' 3. **Enrichment** ([enrich_data()]): Add wind direction factor and lagged variables
#' 4. **Filtering** ([filter_data()]): Keep only rows with complete weather data
#'
#' @param data A tibble with columns:
#'   - `location_id`: Unique location identifier
#'   - `meas_weather`: List-column of tibbles, each containing `date`, `timezone`,
#'      `value` (pollutant measurement), and weather variables
#' @param weather_vars Character vector of weather variable names to use for
#'   lagging and completeness filtering (e.g., `c("wd", "ws", "precip")`).
#' @param time_vars Character vector of time-based features to add
#'   (e.g., `c("hour", "weekday", "month")`). Pass `c()` for none.
#' @param lag Integer. Number of lag days to create for each weather variable.
#'   Creates columns named `{var}_lag1`, `{var}_lag2`, etc. Default is 0 (no lag).
#'
#' @return A tibble with the same structure as `data`, where each `meas_weather`
#'   tibble has been cleaned, filled, enriched, and filtered. Locations with
#'   empty results after processing are removed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Prepare data with 1-day lag and weekday feature
#' prepared <- prep_data(
#'   data = meas_weather_data,
#'   weather_vars = c("wd", "ws", "precip", "air_temp_min", "air_temp_max"),
#'   time_vars = c("weekday"),
#'   lag = 1
#' )
#' }
prep_data <- function(data,
                      weather_vars,
                      time_vars,
                      lag = 0) {

  prep_meas_weather <- function(meas_weather) {
    tryCatch({
      meas_weather %>%
        clean_data() %>%
        fill_data() %>%
        enrich_data(lag = lag, weather_vars = weather_vars) %>%
        filter_data(weather_vars = weather_vars) %>%
        utils.add_timevars(add = time_vars)
    }, error = function(e) {
      return(NULL)
    })
  }

  # Process each meas_weather using lapply to avoid quosure issues
  data$meas_weather <- lapply(data$meas_weather, prep_meas_weather)

  # Remove empty configurations
  data <- data[unlist(sapply(data$meas_weather, length)) > 0, ]

  return(data)
}


# =============================================================================
# Internal helper functions
# =============================================================================

#' Clean measurement-weather data
#'
#' Removes problematic values that would cause issues during model training:
#' - Replaces infinite values with NA
#' - Removes outlier measurements using MAD (Median Absolute Deviation) filter
#' - Converts NaN to NA (required for GBM compatibility)
#' - Removes rows with missing dates
#'
#' @param tbl A tibble with `date`, `value`, and weather columns.
#'
#' @return Cleaned tibble with outliers and invalid values removed.
#'
#' @details
#' The MAD filter removes rows where `value` exceeds 10 × MAD above the median.
#' This is effective for removing extreme pollution events (e.g., sand storms).
#'
#' @keywords internal
clean_data <- function(tbl) {


  # Remove infinite values in all numeric columns
  tbl <- tbl %>%
    mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x))

  # Apply MAD filtering to pollutant peaks (useful for e.g., sand storms in Beijing)
  n_mad <- 10

mad <- stats::mad(tbl$value, na.rm = TRUE)
  n_before <- nrow(tbl)

  tbl <- tbl %>%
    filter(value - median(value, na.rm = TRUE) < n_mad * mad)

  n_after <- nrow(tbl)
  if (n_before != n_after) {
    threshold <- n_mad * mad + median(tbl$value, na.rm = TRUE)
    message(sprintf("%d/%d outliers removed (threshold: %.1f)", n_before - n_after, n_before, threshold))
  }

  # Replace NaN with NA (GBM doesn't handle NaN) and remove rows with missing dates
  tbl <- tbl %>%
    utils.replace_nan_with_na() %>%
    dplyr::filter(!is.na(date))

  return(tbl)
}


#' Fill missing weather values
#'
#' Applies different strategies to handle missing values in weather variables:
#'
#' 1. **All-NA to zero**: If a variable is entirely NA, fill with 0
#' 2. **NA to zero**: For variables matching `zero_fill_pattern`, replace NA with 0
#' 3. **Interpolation**: For variables matching `interpolate_pattern`, interpolate gaps
#'
#' @param tbl A tibble with weather columns.
#' @param zero_fill_pattern Regex pattern for variables where NA should become 0.
#'   Default matches precipitation and fire variables: `"^(precip|fire_)"`.
#' @param interpolate_pattern Regex pattern for variables to interpolate.
#'   Default matches temperature, pressure, and boundary layer variables.
#' @param max_interpolate_gap Maximum gap (in days) to interpolate. Default is 5.
#'
#' @return Tibble with missing values filled according to variable type.
#'
#' @keywords internal
fill_data <- function(
    tbl,
    zero_fill_pattern = "^(precip|fire_)",
    interpolate_pattern = "^(sunshine|atmos_pres|air_temp|ceil_hgt|pbl_|dewpoint_temp)",
    max_interpolate_gap = 5
) {

  weather_cols <- setdiff(names(tbl), c("date", "timezone", "value"))

  # Treatment 1: If entirely NA, replace with 0
  # Prevents models from failing on completely missing variables
  allnatozero <- function(v) {
    if (all(is.na(v))) v[is.na(v)] <- 0
    v
  }

  tbl <- tbl %>%
    mutate_at(weather_cols, allnatozero)

  # Treatment 2: Replace NA with 0 for variables matching pattern
  # Physical interpretation: no rain/fire when not recorded
  vars_natozero <- grep(zero_fill_pattern, weather_cols, value = TRUE)

  natozero <- function(v) {
    v[is.na(v)] <- 0
    v
  }

  tbl <- tbl %>%
    mutate_at(vars_natozero, natozero)

  # Treatment 3: Interpolate continuous variables
  vars_interpolate <- grep(interpolate_pattern, weather_cols, value = TRUE)

  interpolate <- function(v, date) {
    zoo::na.approx(v, date, na.rm = FALSE, maxgap = max_interpolate_gap)
  }

  tbl <- tbl %>%
    mutate_at(vars_interpolate, ~ interpolate(.x, date = date))

  return(tbl)
}


#' Enrich data with derived features
#'
#' Adds features derived from existing variables:
#' - Wind direction factor (8 sectors of 45° each)
#' - Lagged versions of weather variables
#'
#' @param tbl A tibble with weather columns including `wd` (wind direction).
#' @param lag Integer. Number of lag days to create.
#' @param weather_vars Character vector of variables to lag.
#'
#' @return Tibble with additional `wd_factor` and `{var}_lag{n}` columns.
#'
#' @details
#' Wind direction is converted to a factor with 8 levels (0-7), representing
#' 45° sectors: 0 = N (337.5°-22.5°), 1 = NE (22.5°-67.5°), etc.
#'
#' @keywords internal
enrich_data <- function(tbl, lag, weather_vars) {

  # Wind direction factor: 8 sectors of 45° each
  tbl$wd_factor <- factor(tbl$wd %/% 45)

  # Add lagged variables if requested
  if (!is.null(lag) && lag > 0) {
    day_lags <- seq_len(lag)
    tbl <- utils.add_lag(tbl, weather_vars, group_cols = c(), day_lags, "day")
  }

  return(tbl)
}


#' Filter data for model training
#'
#' Applies quality filters to ensure data is suitable for training:
#' - Requires data after a minimum date
#' - Removes rows with missing weather variables
#'
#' @param tbl A tibble with weather columns.
#' @param weather_vars Character vector of required weather variables.
#' @param min_date Minimum date required in the data. If no data exists after
#'   this date, returns NA. Default is "2020-01-01".
#'
#' @return Filtered tibble, or NA if no data exists after `min_date`.
#'
#' @keywords internal
filter_data <- function(tbl, weather_vars, min_date = "2020-01-01") {

  # Require data after min_date
 if (max(tbl$date, na.rm = TRUE) < min_date) {
    warning(sprintf("No measurements available since %s. Returning NA", min_date))
    return(NA)
  }

  # Keep only rows with complete weather data (lagged columns not required)
  tbl <- tbl %>%
    filter(complete.cases(select(., all_of(weather_vars))))

  return(tbl)
}
