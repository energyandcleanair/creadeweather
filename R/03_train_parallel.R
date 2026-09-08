# =============================================================================
# Parallel backend helpers for model training
#
# Training a deweathering model for one (location, pollutant) pair is
# independent of every other pair, so the outer loop over those pairs is
# embarrassingly parallel. gbm3 also parallelises *inside* a single fit via
# OpenMP (`gbmParallel(num_threads=)`), but that path has a hard ceiling: on
# production data it measures ~91% parallel, so efficiency falls to ~65% at 7
# threads and ~44% at 15. Spreading the same cores across processes instead
# measures ~91-95% efficiency, because there is no shared serial section.
#
# These helpers therefore prefer process-level parallelism and only hand
# spare cores to gbm3's threads when there are fewer models than cores
# (which happens when deweather() is called one location at a time).
# =============================================================================


#' Split an available core budget between worker processes and gbm threads
#'
#' Processes are allocated first because they parallelise more efficiently
#' than gbm3's in-fit threading; whatever is left over is given to each
#' worker's gbm threads so a small number of models can still use the box.
#'
#' @param n_items Number of independent models to train.
#' @param n_cores Total core budget. `NULL` uses [future::availableCores()],
#'   which respects container CPU limits (e.g. Cloud Run).
#'
#' @return A list with `workers` (processes) and `threads` (gbm threads each).
#'
#' @keywords internal
dw_split_cores <- function(n_items, n_cores = NULL) {
  if (is.null(n_cores)) {
    n_cores <- tryCatch(
      as.integer(future::availableCores()),
      error = function(e) 1L
    )
  }
  n_cores <- max(1L, as.integer(n_cores))
  n_items <- max(1L, as.integer(n_items))

  workers <- min(n_cores, n_items)
  threads <- max(1L, n_cores %/% workers)

  list(workers = as.integer(workers), threads = as.integer(threads))
}


#' Create a parallel backend for pbapply, on both Unix and Windows
#'
#' pbapply accepts either an integer `cl` (forking, Unix-only - integers are
#' silently ignored on Windows) or a cluster object (works everywhere). We
#' pick whichever is appropriate so the same call site parallelises on both
#' platforms.
#'
#' Falls back to serial execution if a cluster cannot be created, so a
#' parallel backend failure degrades performance rather than breaking the run.
#'
#' @param workers Number of worker processes. 1 or less means serial.
#'
#' @return A list with `cl` (to pass to pbapply) and `close()` to release it.
#'
#' @keywords internal
dw_parallel_backend <- function(workers) {
  serial <- list(cl = NULL, close = function() invisible(NULL))

  if (is.null(workers) || is.na(workers) || workers <= 1L) {
    return(serial)
  }

  if (.Platform$OS.type != "windows") {
    # Forking: workers inherit the session, nothing to export.
    return(list(cl = as.integer(workers), close = function() invisible(NULL)))
  }

  # Windows has no fork(), so use a socket cluster. Workers start empty, so
  # the package has to be loaded in each one before it can train anything.
  cl <- tryCatch(parallel::makePSOCKcluster(workers), error = function(e) NULL)
  if (is.null(cl)) {
    warning("Could not start a parallel cluster; training serially")
    return(serial)
  }

  loaded <- tryCatch({
    parallel::clusterEvalQ(cl, {
      suppressMessages(library(creadeweather))
      suppressMessages(library(gbm3))
    })
    TRUE
  }, error = function(e) FALSE)

  if (!loaded) {
    try(parallel::stopCluster(cl), silent = TRUE)
    warning("Could not load creadeweather on cluster workers; training serially")
    return(serial)
  }

  list(cl = cl, close = function() try(parallel::stopCluster(cl), silent = TRUE))
}
