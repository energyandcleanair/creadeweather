#' Training a model using standard gmb and comparing observed vs predicted as anomaly
#'
#'
#' The data is divided in three sets.
#' A training period that is data fed to GBM
#' (e.g. three years before the period we want deweathered data from when using 
#' anomaly approach)
#' 
#' This training period is itself divided into:
#'  - training GBM per se (`training.fraction`)
#'  - validation by GBM (1-`training.fraction`)
#'
#' 
#' A prediction period which is the period for which we want to get deweathered data
#' (when using anomaly approach)
#'
#' 
#' @param location_id 
#' @param data 
#' @param pollutant 
#' @param unit 
#' @param training_end
#' @param link: either null or 'log'
#'
#' @return
#' @export
#'
#' @examples
train_gbm <- function(data,
                      training_end,
                      weather_vars,
                      time_vars,
                      trees,
                      normalise,
                      detect_breaks,
                      samples,
                      interaction.depth = 1,
                      learning.rate = 0.1,
                      link = "linear",
                      training.fraction = 0.9,
                      cv_folds = 3,
                      ...){

  prepared <- train_gbm_prepare_data(
    data = data,
    training_end = training_end,
    weather_vars = weather_vars,
    time_vars = time_vars,
    link = link,
    training.fraction = training.fraction
  )

  train_gbm_fit_model(
    data_prepared = prepared$data,
    formula = prepared$formula,
    trees = trees,
    interaction.depth = interaction.depth,
    learning.rate = learning.rate,
    cv_folds = cv_folds
  )
}



#' @keywords internal
train_gbm_prepare_data <- function(data,
                                   training_end,
                                   weather_vars,
                                   time_vars,
                                   link = "linear",
                                   training.fraction = 0.9,
                                   shuffle_seed = 42) {
  if (is.null(training_end)) {
    training_end <- "2099-01-01"
  }

  if (!link %in% c("linear", "log")) {
    stop("link can only be 'linear' or 'log'")
  }

  do_link <- if (link == "linear") {
    function(x) x
  } else {
    function(x) log(x)
  }

  do_unlink <- if (link == "linear") {
    function(x) x
  } else {
    function(x) exp(x)
  }

  if ("geometry" %in% names(data)) {
    data <- data[, setdiff(names(data), "geometry"), drop = FALSE]
  }

  data_prepared <- data
  data_prepared$date <- as.POSIXct(data_prepared$date)

  if (training.fraction < 1) {
    set.seed(shuffle_seed)
    rows <- sample(nrow(data_prepared))
    data_prepared <- data_prepared[rows, , drop = FALSE]
  }

  # We separate into:
  # Training: before date_cut, taking a training.fraction share
  # Testing: before date_cut, taking a 1-training.fraction share
  # Prediction: after date_cut (typically for anomaly estimation: observed-predicted)
  training_end_posix <- as.POSIXct(training_end)

  i_before_cut <- which(data_prepared$date <= training_end_posix)
  if (length(i_before_cut) == 0) {
    stop("Missing training data")
  }

  training_size <- max(1, floor(training.fraction * length(i_before_cut)))
  i_training <- sample(i_before_cut, training_size)
  i_testing <- setdiff(i_before_cut, i_training)
  i_prediction <- setdiff(seq_len(nrow(data_prepared)), i_before_cut)

  data_prepared$set <- NA_character_
  data_prepared$set[i_training] <- "training"
  data_prepared$set[i_testing] <- "testing"
  data_prepared$set[i_prediction] <- "prediction"

  formula_vars <- c(time_vars, weather_vars)
  training_subset <- data_prepared[data_prepared$set == "training", , drop = FALSE]
  available_formula_vars <- formula_vars[vapply(formula_vars, function(var) {
    var %in% names(training_subset) && any(!is.na(training_subset[[var]]))
  }, logical(1))]

  formula <- reformulate(termlabels = available_formula_vars, response = "value")

  if (length(available_formula_vars) > 0) {
    keep_rows <- apply(!is.na(data_prepared[, available_formula_vars, drop = FALSE]), 1, any)
  } else {
    keep_rows <- rep(TRUE, nrow(data_prepared))
  }

  keep_rows <- keep_rows & !is.na(data_prepared$value)
  data_prepared <- data_prepared[keep_rows, , drop = FALSE]
  data_prepared$value <- do_link(data_prepared$value)

  list(
    data = data_prepared,
    formula = formula,
    do_link = do_link,
    do_unlink = do_unlink
  )
}

#' @keywords internal
train_gbm_fit_model <- function(data_prepared,
                                formula,
                                trees,
                                interaction.depth = 1,
                                learning.rate = 0.1,
                                cv_folds = 3) {
  if (!"set" %in% names(data_prepared)) {
    stop("Prepared data must include a 'set' column")
  }

  training_rows <- data_prepared$set == "training" & !is.na(data_prepared$value) & !is.infinite(data_prepared$value)
  training_data <- data_prepared[training_rows, , drop = FALSE]

  if (nrow(training_data) == 0) {
    stop("Missing training data")
  }

  model_gbm <- function(training_data, formula) {
    message("Training gbm")
    gbm::gbm(
      formula = formula,
      data = training_data,
      distribution = "gaussian",
      cv.folds = cv_folds,
      shrinkage = learning.rate,
      interaction.depth = interaction.depth,
      train.fraction = 1,
      n.cores = 1,
      n.trees = trees,
      verbose = FALSE,
      keep.data = FALSE
    )
  }

  model <- model_gbm(training_data, formula)
  n.trees.opt <- gbm::gbm.perf(model, method = "cv", plot.it = FALSE)
  message(sprintf("Using %d trees (based on CV results)", n.trees.opt))

  tibble::tibble(
    model = list(model),
    data = list(data_prepared)
  )
}



