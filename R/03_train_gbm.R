#' Train a Gradient Boosting Model for deweathering
#'
#' Trains a GBM model to predict pollutant concentrations from weather and time
#' variables. The model can then be used to estimate weather-normalized pollution
#' levels (deweathering).
#'
#' @section Data Partitioning:
#' The data is divided into three sets based on `training_end`:
#'
#' **Before `training_end`** (split by `training.fraction`):
#' - **Training set**: Used to fit the GBM model
#' - **Testing set**: Used for validation (not seen during training)
#'
#' **After `training_end`**:
#' - **Prediction set**: Used for anomaly estimation (observed - predicted)
#'
#' @param data A data frame with columns: `date`, `value` (pollutant measurement),
#'   and weather/time variables specified in `weather_vars` and `time_vars`.
#' @param training_end Date or string. Cutoff date for the training period.
#'   Data before this date is used for training/testing; data after is for prediction.
#' @param weather_vars Character vector of weather variable names to use as predictors
#'   (e.g., `c("ws", "wd", "air_temp_min", "precip")`).
#' @param time_vars Character vector of time-based predictors
#'   (e.g., `c("weekday", "month", "trend")`).
#' @param trees Integer. Maximum number of trees for the GBM. The optimal number
#'   is determined via cross-validation or OOB.
#' @param normalise Logical. Whether to normalize predictors (currently unused).
#' @param detect_breaks Logical. Whether to detect structural breaks (currently unused).
#' @param samples Integer. Number of samples for uncertainty estimation (currently unused).
#' @param training_excluded_dates Date vector. Specific dates to exclude from training
#'   (e.g., known anomalous events). These dates remain in the testing set.
#' @param interaction.depth Integer. Maximum depth of variable interactions.
#'   Default is 1 (additive model).
#' @param learning.rate Numeric. Shrinkage parameter for GBM. Smaller values
#'   require more trees but often give better results. Default is 0.1.
#' @param link Character. Link function: `"linear"` (default) or `"log"`.
#'   Whether we want to train on the actual value or on the log of the value.
#' @param training.fraction Numeric between 0 and 1. Fraction of pre-cutoff data
#'   used for training (rest is testing). Default is 0.9.
#' @param cv_folds Integer. Number of cross-validation folds for determining
#'   optimal tree count. Use 1 for OOB estimation. Default is 3.
#' @param parallel Logical. Whether to use parallel processing. Default is TRUE.
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble with one row containing:
#'   - `model`: The fitted GBM model object
#'   - `data`: The prepared data with `set` and `predicted` columns
#'   - `performance`: List with RMSE and R² for each set
#'
#' @export
#'
#' @examples
#' \dontrun
#' result <- train_gbm(
#'   data = meas_weather,
#'   training_end = "2020-12-31",
#'   weather_vars = c("ws", "wd", "air_temp_min", "precip"),
#'   time_vars = c("weekday", "month"),
#'   trees = 1000,
#'   training.fraction = 0.9,
#'   cv_folds = 3
#' )
#' }
train_gbm <- function(data,
                      training_end,
                      weather_vars,
                      time_vars,
                      trees,
                      normalise,
                      detect_breaks,
                      samples,
                      training_excluded_dates = c(),
                      interaction.depth = 1,
                      learning.rate = 0.1,
                      link = "linear",
                      training.fraction = 0.9,
                      cv_folds = 3,
                      parallel = TRUE,
                      ...) {

  prepared <- train_gbm_prepare_data(
    data = data,
    training_end = training_end,
    weather_vars = weather_vars,
    time_vars = time_vars,
    link = link,
    training.fraction = training.fraction,
    training_excluded_dates = training_excluded_dates
  )

  train_gbm_fit_model(
    data_prepared = prepared$data,
    formula = prepared$formula,
    trees = trees,
    interaction.depth = interaction.depth,
    learning.rate = learning.rate,
    cv_folds = cv_folds,
    parallel = parallel
  )
}

get_model_performance <- function(model, data_prepared){
  
  perf <- lapply(split(data_prepared, data_prepared$set), function(d){
    tibble(
      set=unique(d$set),
      rmse=Metrics::rmse(d$value, d$predicted),
      rsquared=cor(d$value, d$predicted)^2
    )
  }) %>%
    do.call(bind_rows, .) %>%
    tidyr::pivot_wider(names_from=set, values_from=c(rmse, rsquared)) %>%
    as.list()
}

# =============================================================================
# Internal helper functions
# =============================================================================

#' Prepare data for GBM training
#'
#' Partitions data into training, testing, and prediction sets, applies the
#' link function, and builds the model formula.
#'
#' @param data Data frame with `date`, `value`, and predictor columns.
#' @param training_end Cutoff date for training period. NULL defaults to "2099-01-01".
#' @param weather_vars Character vector of weather predictor names.
#' @param time_vars Character vector of time predictor names.
#' @param link Link function: "linear" or "log".
#' @param training.fraction Fraction of pre-cutoff data for training.
#' @param training_excluded_dates Dates to exclude from training set.
#' @param shuffle_seed Random seed for reproducible shuffling. Default is 42.
#'
#' @return A list containing:
#'   - `data`: Prepared data frame with `set` column ("training"/"testing"/"prediction")
#'   - `formula`: Model formula object
#'   - `do_link`: Link function
#'   - `do_unlink`: Inverse link function
#'
#' @keywords internal
train_gbm_prepare_data <- function(data,
                                   training_end,
                                   weather_vars,
                                   time_vars,
                                   link = "linear",
                                   training.fraction = 0.9,
                                   training_excluded_dates = c(),
                                   shuffle_seed = 42) {

  # Handle NULL training_end (use far future date)
  if (is.null(training_end)) {
    training_end <- "2099-01-01"
  }

  # Validate and create link functions
  if (!link %in% c("linear", "log")) {
    stop("link must be 'linear' or 'log'")
  }

  do_link <- if (link == "linear") identity else log
  do_unlink <- if (link == "linear") identity else exp

  # Remove geometry column if present (from sf objects)
  if ("geometry" %in% names(data)) {
    data <- data %>% dplyr::select(-c(geometry))
  }

  # Convert date to POSIXct for consistent handling
  data_prepared <- data %>%
    mutate(date = as.POSIXct(date))

  # Shuffle data (important: GBM doesn't shuffle internally)
  if (training.fraction < 1) {
    set.seed(shuffle_seed)
    rows <- sample(nrow(data_prepared))
    data_prepared <- data_prepared[rows, ]
  }

  # We separate into:
  # Training: before date_cut, taking a training.fraction share
  # Testing: before date_cut, taking a 1-training.fraction share
  # Prediction: after date_cut (typically for anomaly estimation: observed-predicted)
  training_end_posix <- as.POSIXct(training_end)
  i_before_cut <- which(data_prepared$date <= training_end_posix)

  if (length(i_before_cut) == 0) {
    stop("No data available before training_end date")
  }

  # Handle excluded dates
  date_only <- as.Date(data_prepared$date)
  excluded_dates <- unique(stats::na.omit(as.Date(training_excluded_dates)))

  i_excluded <- if (length(excluded_dates) > 0) {
    which(date_only %in% excluded_dates)
  } else {
    integer(0)
  }

  # Calculate training set indices
  training_candidates <- setdiff(i_before_cut, i_excluded)

  if (length(training_candidates) == 0) {
    stop("No training data available after excluding requested dates")
  }

  target_training_size <- max(1, floor(training.fraction * length(i_before_cut)))
  training_size <- min(length(training_candidates), target_training_size)

  if (training_size < target_training_size) {
    warning(
      "Insufficient data after excluding dates. ",
      "Effective training fraction reduced to ",
      round(training_size / length(i_before_cut), 2)
    )
  }

  # Assign set labels
  i_training <- sample(training_candidates, training_size)
  i_testing <- setdiff(i_before_cut, i_training)
  i_prediction <- setdiff(seq_len(nrow(data_prepared)), i_before_cut)

  data_prepared$set <- NA_character_
  data_prepared$set[i_training] <- "training"
  data_prepared$set[i_testing] <- "testing"
  data_prepared$set[i_prediction] <- "prediction"

  # Build formula from available variables
  formula_vars <- c(time_vars, weather_vars)
  training_subset <- data_prepared[data_prepared$set == "training", , drop = FALSE]

  available_formula_vars <- formula_vars[vapply(formula_vars, function(var) {
    var %in% names(training_subset) && any(!is.na(training_subset[[var]]))
  }, logical(1))]

  formula <- reformulate(termlabels = available_formula_vars, response = "value")

  # Filter rows with at least some non-NA predictors and non-NA response
  if (length(available_formula_vars) > 0) {
    keep_rows <- apply(!is.na(data_prepared[, available_formula_vars, drop = FALSE]), 1, any)
  } else {
    keep_rows <- rep(TRUE, nrow(data_prepared))
  }

  keep_rows <- keep_rows & !is.na(data_prepared$value)
  data_prepared <- data_prepared[keep_rows, , drop = FALSE]

  # Apply link function to response
  data_prepared$value <- do_link(data_prepared$value)

  list(
    data = data_prepared,
    formula = formula,
    do_link = do_link,
    do_unlink = do_unlink
  )
}


#' Fit GBM model to prepared data
#'
#' Trains the GBM model, determines optimal tree count via CV or OOB,
#' and generates predictions for all data sets.
#'
#' @param data_prepared Data frame from [train_gbm_prepare_data()] with `set` column.
#' @param formula Model formula object.
#' @param trees Maximum number of trees.
#' @param interaction.depth Maximum interaction depth.
#' @param learning.rate Shrinkage parameter.
#' @param cv_folds Number of CV folds (use 1 for OOB).
#' @param parallel Whether to use parallel processing.
#'
#' @return A tibble with columns: `model`, `data`, `performance`.
#'
#' @keywords internal
train_gbm_fit_model <- function(data_prepared,
                                formula,
                                trees,
                                interaction.depth = 1,
                                learning.rate = 0.1,
                                cv_folds = 3,
                                parallel = TRUE) {

  if (!"set" %in% names(data_prepared)) {
    stop("Prepared data must include a 'set' column")
  }

  # Extract training data
  training_rows <- data_prepared$set == "training" &
    !is.na(data_prepared$value) &
    !is.infinite(data_prepared$value)
  training_data <- data_prepared[training_rows, , drop = FALSE]

  if (nrow(training_data) == 0) {
    stop("No valid training data available")
  }

  # Configure parallelization
  n_cores <- if (parallel) as.integer(future::availableCores() - 1) else 1L

  # Train GBM model
  message("Training GBM model...")
  model <- gbm3::gbm(
    formula = formula,
    data = training_data,
    distribution = "gaussian",
    cv.folds = cv_folds,
    shrinkage = learning.rate,
    interaction.depth = interaction.depth,
    train.fraction = 1,  # We handle train/test split separately
    par.details = gbm3::gbmParallel(num_threads = n_cores),
    n.trees = trees,
    verbose = FALSE,
    keep.data = FALSE
  )

  # Determine optimal number of trees
  if (cv_folds > 1) {
    n_trees_opt <- gbm3::gbm.perf(model, method = "cv", plot.it = FALSE)
    message(sprintf("Optimal trees: %d (CV)", n_trees_opt))
  } else {
    n_trees_opt <- gbm3::gbm.perf(model, method = "OOB", plot.it = FALSE)
    message(sprintf("Optimal trees: %d (OOB)", n_trees_opt))
  }
  model$n.trees.opt <- n_trees_opt

  # Generate predictions for all sets
  data_prepared$predicted <- predict(model, data_prepared, n.trees = n_trees_opt)

  tibble(
    model = list(model),
    data = list(data_prepared),
    performance = list(get_model_performance(model, data_prepared))
  )
}


#' Calculate model performance metrics
#'
#' Computes RMSE and R² for each data set (training, testing, prediction).
#'
#' @param model The fitted GBM model (unused, kept for potential future use).
#' @param data_prepared Data frame with `set`, `value`, and `predicted` columns.
#'
#' @return A named list with `rmse_*` and `rsquared_*` for each set.
#'
#' @keywords internal
get_model_performance <- function(model, data_prepared) {

  perf <- lapply(split(data_prepared, data_prepared$set), function(d) {
    tibble(
      set = unique(d$set),
      rmse = Metrics::rmse(d$value, d$predicted),
      rsquared = cor(d$value, d$predicted)^2
    )
  }) %>%
    do.call(bind_rows, .) %>%
    tidyr::pivot_wider(names_from = set, values_from = c(rmse, rsquared)) %>%
    as.list()

  return(perf)
}
