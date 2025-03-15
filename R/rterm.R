#' Generate a random term in a model.
#'
#' For demonstration purposes, create a model term with
#' `df` degrees of freedom consisting of `df` random vectors.
#'
#' @param df Degrees of freedom of the term: positive integer
#' @param seed An (optional) seed for the random number generator: integer
#' @param rdist The random number generator to use. Default `rdist`.
#' @param args A list of parameters for the function named in `rdist`.
#' @param nrow number of rows in the model term. This is determined automatically
#' when used in a function like `lm()` or `model_train()`.
#'
#' @examples
#' lm(mpg ~ hp + rterm(df=25, seed=29323), data = mtcars) |> R2()
#'
#' @export
rterm <- function (df = 1, rdist = rnorm, args = list(), nrow, seed = NULL)
{
  if (missing(nrow)) {
    nrow <- length(get(ls(envir = parent.frame())[1], envir = parent.frame()))
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  result <- matrix(do.call(rdist, args = c(list(n = df * nrow),
                                           args)), nrow = nrow)
  return(result)
}
