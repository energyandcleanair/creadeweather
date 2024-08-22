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
                      training_excluded_dates=c(),
                      interaction.depth=1,
                      learning.rate=0.1,
                      link="linear",
                      training.fraction=0.9,
                      cv_folds=3,
                      ...){
  
  if(is.null(training_end)){
    training_end <- "2099-01-01"
  }
  
  n_cores <- as.integer(future::availableCores()-1)
  
  if(!link %in% c('linear','log')){
    stop("link can only be 'linear' or 'log'")
  }
  
  if(link=="linear"){
    do_link <- function(x){x}
    do_unlink <- function(x){x}
  }else if(link=="log"){
    do_link <- function(x){log(x)}
    do_unlink <- function(x){exp(x)}
  }
  
  if("geometry" %in% names(data)){
    data <- data %>% dplyr::select(-c(geometry)) 
  }

  # Prepare data ------------------------------------------------------------
  data_prepared <- data %>%
    mutate(date=as.POSIXct(date))
  
  # Reshuffle as it seems GBM doesn't do it
  # Very important
  if(training.fraction<1){
    set.seed(42)
    rows <- sample(nrow(data_prepared))
    data_prepared <- data_prepared[rows, ]
  }
  
  # We separate into:
  # Training: before date_cut, taking a training.fraction share
  # Testing: before date_cut, taking a 1-training.fraction share
  # Prediction: after date_cut (typically for anomaly estimation: observed-predicted)
  i_before_data_cut <- which(data_prepared$date <= training_end)
  i_excluded <- which(data_prepared$date %in% as.Date(training_excluded_dates))
  i_training <- tryCatch({
    sample(setdiff(i_before_data_cut, i_excluded),
           training.fraction * length(i_before_data_cut))  
  }, error=function(e){
    i_training <- setdiff(i_before_data_cut, i_excluded)
    new_training_fraction <- length(i_training) / length(i_before_data_cut)
    warning("Not enough data to train after excluding training dates. Reducing training fraction to ", round(new_training_fraction,2))
    return(i_training)
  })
  i_testing <- setdiff(i_before_data_cut, i_training)
  i_prediction <- which(data_prepared$date > training_end)
  
  data_prepared[i_training, 'set'] <- 'training'
  data_prepared[i_testing, 'set'] <- 'testing'
  data_prepared[i_prediction, 'set'] <- 'prediction'
  
  if(length(i_training)==0){
    stop("Missing training data")
  }
  
  # Train model -------------------------------------------------------------
  
  model_gbm  <- function(training_data, formula){
    print("Training gbm")
    gbm.fit <- gbm::gbm(
      formula = formula,
      data = training_data,
      distribution='gaussian',
      cv.folds = cv_folds,
      shrinkage=learning.rate,
      interaction.depth=interaction.depth,
      train.fraction = 1, # We keep testing set separately
      n.cores = 1, # nc_cores, Reaching C stack usage limit. Trying to see if this changes something
      n.trees = trees, #This is actually the max number of trees. Will adjust after
      verbose = FALSE,
      keep.data = F
    )
    print("Done")
    return(gbm.fit)
  }

  formula_vars <- c(time_vars, weather_vars)
  
  # Some values may be all NA in training only
  formula_vars <- data_prepared %>%
    filter(set=='training') %>%
    summarise_at(formula_vars, function(x) all(is.na(x))) %>%
    tidyr::gather() %>%
    filter(!value) %>%
    pull(key)
  
  formula <- reformulate(termlabels=formula_vars,
                         response='value')
  
  data_prepared <- data_prepared %>%
    dplyr::filter(if_any(formula_vars, ~ !is.na(.))) %>%
    dplyr::filter_at("value", all_vars(!is.na(.)))
  
  
  # Add "link" transformation if required
  data_prepared$value <- do_link(data_prepared$value)
  
  # Fit
  model <- model_gbm(data_prepared %>% dplyr::filter(set=="training" & !is.na(value) & !is.infinite(value)), formula) 
  
  # Optimal number of trees
  # Two options: OOB or CV
  # Apparently, CV is better on large datasets: https://towardsdatascience.com/understanding-gradient-boosting-machines-9be756fe76ab
  n.trees.opt <- gbm::gbm.perf(model, method="cv", plot.it = F)
  print(sprintf("Using %d trees (based on CV results)", n.trees.opt))
  
  
  return(
    tibble(
      model=list(model),
      data=list(data_prepared))
  )
}



