#' Run a datasim simulation, producing a data frame
#'
#' @param sim A simulation object, as produced by `datasim_make()`.
#' @param n The size of the data sample pulled from the simulation.
#' @param seed An integer random number seed, for reproducibility. (Or, use `set.seed()` before
#' running `sim_run()`.)
#' @param report_hidden logical. If `TRUE`, show the values of hidden variables (that is, variables whose name
#' begins with a dot)
#'
#' @returns a data frame with a column for each node in the datasim object.
#' @export
datasim_run <- function(sim, n=5, seed = NULL, report_hidden = FALSE) {
  # a simple utility function
  exo <- function(n, sd = 1) {
    stats::rnorm(n, mean=0, sd=sd)
  }
  # set random number generator seed, if called for
  if (!is.null(seed)) set.seed(seed)

  # create stub for the output
  values <- vector("list", length(sim$names))
  names(values) <- as.character(sim$names)
  values[["n"]] <- n # special variable

  # construct the values
  for (k in seq_along(sim$names)) {
    tmp <- eval(sim$calls[[k]], values)
    if (inherits(tmp, "each-object")) {
      # <tmp> is a function that when evaluated gives a single value
      # in return
      # put the output so far into a convenient form for accessing by row
      if (k > 1) sofar <- tibble::as_tibble(values[1:(k-1)])
      else sofar <- list() # if this is the first variable, don't need any context
      sofar$n <- n # restore this component
      # We don't know the type of the result, so store the n items as a list
      results <- lapply(1:n, function(x) 1) # a list for the results
      for (i in 1:n) {
        # Evaluate the function <tmp> separately for each row
        if (length(sofar) > 0) foo <- tmp(sofar[i,])
        else foo <- tmp(sofar)
        if (length(foo) > 1) stop("Expression in `each()` must return a scalar.")
        results[[i]] <- foo
      }
      values[[k]] <- unlist(results)
    } else {
      # ordinary direct arithmetic on values
      if (length(tmp) < values$n) {
        # if `tmp` is a scalar or short vector, replicate it so that it has `n` entries
        values[[k]] <- rep(tmp, length.out = values$n)
      } else {
        values[[k]] <- tmp
      }
    }
  }

  # Get rid of unwanted names, such as those starting with a dot (".")
  values$n <- NULL
  if (!report_hidden) {
    rid <- grepl("^\\.", names(values))
    if (any(rid)) values[rid] <- NULL
  }

  # return a data frame
  tibble::as_tibble(values)
}

