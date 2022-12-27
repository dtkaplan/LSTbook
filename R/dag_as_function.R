#' Turn a DAG into a function
#'
#' This is an experimental operation. It works just fine, but I'm not sure
#' whether it's needed for any purpose in teaching Math 300R.
#'
#' @param DAG A DAG
#'
#' @returns A function which, when evaluated with a numerical input (the random-number generator
#' seed), will return `n` rows of output from the DAG.
#'
#' @export
dag_as_function <- function(DAG) {
  function(seed, n=1) {
    dag_sample(DAG, size=n, seed=seed)
  }
}
