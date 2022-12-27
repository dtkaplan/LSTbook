#' Construct a DAG object
#'
#' DAG objects are made from formulas. Each formula has a variable name
#' on the left-hand size, and an expression on the right-hand side. The expression is to
#' to be written using variables defined in earlier formulas, as well as random
#' number generators, etc.
#' @details Some random number generators are built in, and can be used in the DAG.
#' - `exo(sd=1)` --- normally distributed with the given standard deviation
#' - `unif(min=0, max=1)` --- uniformly distributed
#' - `tdist(df=3, ncp=0)` --- t-distributed (long tails)
#' - `roll(levels=1:6, weights=1)` --- generate samples from a vector
#' - `each(tilde)` --- run the same instructions anew for each row
#' Also,
#' - `seq()` --- sequence from 1 to nrow
#' - `binom(input)` --- generates a 0/1 variable based on the magnitude of the input. Probability of 1 is a logistic transformation
#' of the input.
#' Use `sample()` to collect data from a DAG. Arguments: `size=` and, optionally, `seed=`
#'
#' @param \dots one or more formulas in the DAG format
#' puts the right class on a dag, so that sample will work.
#' @examples
#' one <- dag_make(x ~ exo(0.5), y ~ x + exo(0.5))
#' two <- dag_make(.genes ~ exo(), x ~ .genes + exo(), y ~ .genes + exo()))
#' three <- dag_make(x ~ c("a", "b", "c"), y ~ 2)) # e.g. for blocking
#' four <- dag_make(x ~ roll(c("a", "b", "c"))))
#' sample(one, size=5)
#' @export
dag_make <- function(...) {
  # TO DO. Check the formulas.
  DAG <- list(...)
  if (!dag_check(DAG)) stop("Variable(s) used but not given formula.")

  res <- dag_sort(DAG)
  # class(res) <- "dagsystem" # This is done in dag_sort

  res
}
