#' Generate data from a DAG
#'
#' A DAG is represented as a list of formulas. The right-hand side
#' gives the name of the variable. Variables starting with dots will not be
#' printed.
#'
#' @param DAG list of formulas. Variables must be listed before they
#' can be used to create other variables.
#' @param size size of the sample, that is, the number of rows to be put in the data frame
#' @param seed Set the random number seed. Useful for reproducibility.
#' @param survive one-sided tilde expression that generates a boolean/logical from the
#' DAG variables indicating whether to keep the case in the output.
#' @param show_hidden If `TRUE`, show even the hidden variables.
#' @param .size_multiplier number (default 10) by which to increase the
#' number of rows initiially generated so that the output size will be that nominally
#' specified by `size=`. Note: If `.size_multiplier` isn't big enough, the output size
#' will be too small.
#' that
#'
#' @examples
#' dag_sample(dag03)
#' dag_sample(dag03, survive= ~ g > 0)
#' @importFrom tibble as_tibble
#' @export
dag_sample <- function(DAG, size=10, seed=NULL, survive=NULL, show_hidden=FALSE, .size_multiplier=10) {
  # check that DAG is a list of formulas
  if (!is.list(DAG)) stop("DAG must be a list of formulas")
  if (!all(unlist(lapply(DAG, function(x) inherits(x, "formula")))))
    stop("All the components of DAG must be formulas.")

  out_size <- size
  survived <- TRUE # a boolean TRUE (default: keep all the rows)
  size <- ifelse(!is.null(survive), .size_multiplier*out_size, out_size)

  # random noise generators
  exo <- eps <- function(sd = 1) {
    rnorm(size, mean=0, sd=sd)
  }
  tdist <- function(df=3, ncp=0) {
    rt(size, df=df, ncp=ncp)
  }
  unif <- function(min=0, max=1) {
    runif(size, min=min, max=max)
  }
  roll <- function(levels=1:6, weight=rep(1, length(levels))) {
    replicate(size, sample(levels, size=1, prob=weight))
  }
  each <- function(expr) {
    expr <- substitute(expr)
    replicate(size, eval(expr))
  }
  cumulative <- function(probs, vals) {
    cumprobs <- cumsum(probs/(sum(probs)))
    pick <- runif(size)
    choices <- outer(pick, cumprobs, FUN=`>=`) |>
      apply(1, function(x) max(which(x)))
    vals[choices]
  }
  #coin flips
  count_flips <- function(x, prob=.5) {
    rbinom(size, size=x, prob)
  }
  #transformations
  binom <- function(x=0, labels=NULL) {
    # 1 or 0 output with logistic input
    prob <- exp(x)/(1+exp(x))
    yesno <- as.numeric(runif(size) < prob)
    if (!is.null(labels) && length(labels)==2)
      yesno <- labels[yesno+1]

    yesno
  }
  seq <- function() 1:size

  # set random number generator seed, if called for
  if (!is.null(seed)) set.seed(seed)

  vnames <- lapply(DAG, function(x) all.names(x[[2]])) |> unlist()

  Res <- list()

  # make sure they are sorted to depend only on the rows above.

  for (k in 1:length(DAG)) {
    # Carry out the operation assumed by the formula
    rhs <- rlang::f_rhs(DAG[[k]])

    this <- eval(rhs, envir = Res)

    # next line is to handle constants, e.g. x ~ 3
    if (length(this) != size) this <- rep_len(this, size)

    Res[[vnames[k]]] <- this # make it available for successive formulas.
  }

  Res <- tibble::as_tibble(Res) # convert to a data frame
  if (!is.null(survive)) {
    rhs <- rlang::f_rhs(survive)
    survived <- eval(rhs, envir=Res)
    Res <- Res %>%
      filter(survived) %>%
      slice_sample(n=min(out_size, nrow(.)))
  }

  # Post-process: take out the items whose names start with dots
  rid <- rep(FALSE, length(vnames))
  if (! show_hidden) rid <- grepl("^\\.", vnames)

  return(Res[, !rid])
}

#' @importFrom mosaic sample
#' @export
sample.dagsystem <- function(x, size, replace = FALSE, ...) {
  if (missing(size)) size=5
  dag_sample(x, size=size, ...)
}

#' @export
print.dagsystem <- function(x, ..., show_hidden=FALSE) {
  if (!show_hidden) {
    # find the hidden ones and suppress
    left_names <- lapply(x, FUN=function(x) all.vars(x[[2]]))
    rid <- which(grepl("^\\.", left_names))
    if (length(rid) > 0) x <- x[-rid]
  }
  cat(paste(unlist(lapply(x, FUN=capture.output)),
        collapse="\n"))
}




