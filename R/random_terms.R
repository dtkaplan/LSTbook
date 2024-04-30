#' create columns with random numbers
#'
#' For demonstration purposes, add the specified number of
#' random columns to a model matrix. This is intended to be used
#' in modeling functions, e.g. `model_train()`, `lm()`, and so on
#' to explore the extent to which random columns "explain" the
#' response variable.
#'
#' @param df How many columns to add
#' @param rdist Function to generate each column's numbers (default: `rnorm`)
#' @param args A list holding the parameters (if any) to be used for the `rdist` argument
#' @param n OPTIONALLY, dictate the number of rows in the output
#' @param seed Integer seed for the random-number generator
#'
#' @details `random_terms()` will try to guess a suitable value for `n` based on
#' the calling function.
#'
#' @examples
#'  mtcars |> model_train(mpg ~ wt + random_terms(4)) |> conf_interval()
#'  mtcars |> model_train(mpg ~ wt + random_terms(4)) |> anova_report()
#'  head(mtcars) |> select(wt, mpg) |> mutate(r = random_terms(3))
#' @export
random_terms <- function (df = 1, rdist = rnorm, args = list(), n, seed = NULL)
{
  if (missing(n)) {
    arg <- sys.call(1)[[2]]
    # walk down the stack until reaching the first argument to this function
    # that won't be a recursive call involving "random_terms"
    while("random_terms" %in% all.names(arg)) arg <- arg[[2]]
    # if it's a data frame, we have our answer.
    if (inherits(eval(arg), "data.frame")) n = nrow(eval(arg))
    else stop("Need to specify argument <n> in random_terms().")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  result <- matrix(do.call(rdist, args = c(list(n = df * n),
                                           args)), nrow = n)
  return(result)
}
