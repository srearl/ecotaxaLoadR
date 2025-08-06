#' @title Set up parallel processing for ecotaxaLoadR functions
#'
#' @param workers Number of parallel workers. Defaults to available cores - 1.
#'
#' @importFrom future availableCores plan multisession
#'
#' @export
setup_parallel_ecotaxa <- function(workers = future::availableCores() - 1) {
  # Check if already using multisession with same workers
  current_plan <- future::plan()
  if (inherits(current_plan, "multisession") && 
      length(current_plan$workers) == workers) {
    cat("Already using", workers, "workers. No change needed.\n")
    return(invisible())
  }
  
  future::plan(future::multisession, workers = workers)
  cat("Set up parallel processing with", workers, "workers\n")
  cat("Functions using parallel processing: annotate_daytime()\n")
  cat("Use reset_parallel_ecotaxa() to return to sequential processing\n")
}

#' @title Reset to sequential processing
#'
#' @importFrom future plan sequential
#'
#' @export
reset_parallel_ecotaxa <- function() {
  future::plan(future::sequential)
  cat("Reset to sequential processing\n")
  cat("All ecotaxaLoadR functions will now run sequentially\n")
}

#' @title Check current parallel setup
#'
#' @importFrom future plan
#'
#' @export
check_parallel_ecotaxa <- function() {
  current_plan <- future::plan()
  if (inherits(current_plan, "sequential")) {
    cat("Currently using sequential processing (no parallelization)\n")
  } else if (inherits(current_plan, "multisession")) {
    workers <- length(current_plan$workers)
    cat("Currently using parallel processing with", workers, "workers\n")
  } else {
    cat("Currently using:", class(current_plan)[1], "parallel backend\n")
  }
}