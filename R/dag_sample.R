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
#' @param report_hidden If `TRUE`, show even the hidden variables.
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
dag_sample <- function(DAG, size=10, seed=NULL, survive=NULL, report_hidden=FALSE, .size_multiplier=10) {
  # check that DAG is a list of formulas
  if (!is.list(DAG)) stop("DAG must be a list of formulas")
  if (!all(unlist(lapply(DAG, function(x) inherits(x, "formula")))))
    stop("All the components of DAG must be formulas.")

  survived <- TRUE # a boolean TRUE (default: keep all the rows)
  size <- ifelse(!is.null(survive), .size_multiplier*size, size)

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
  pois <- function(rate) {
    rpois(size, lambda=rate)
  }
  roll <- function(levels=1:6, weight=rep(1, length(levels))) {
    replicate(size, sample(levels, size=1, prob=weight))
  }
  each <- function(expr) {
    expr <- substitute(expr)
    replicate(size, eval(expr))
  }
  # block an assignment to various levels by another variable
  block_by <- function(block_var, levels=c("treatment", "control")) {
    orig <- 1:size
    inds <- order(block_var)
    block_var <- block_var[inds]
    if (is.numeric(block_var)) {
      # divide into even-sized groups
      block_var <- ((1:size)-1) %/% length(levels)
    }
    orig <- orig[inds]
    out <- rep_len(levels, length.out=size)
    out <- mosaic::sample(out, groups=block_var) # randomize the order within the blocks
    back_inds <- order(orig)
    out[back_inds]
  }
  # translate each level of a categorical variable into a specified numerical value
  value <- function(variable, levels, vals) {
    names(vals) <- levels
    as.vector(vals[variable])
  }


  # many levels
  categorical <- function(levels = c("A", "B", "C"), exact=TRUE, probs = rep(1, length(levels))) {
    probs <- probs/sum(probs) # They should add up to 1
    if (exact) { # table of counts should be as exactly as possible matched to probs.
      if (length(unique(probs)) != 1)
        return(sample(rep.int(levels, times=round(probs*size))))
      else
        return(sample(rep_len(levels, length.out=size)))
    }
    # When <exact> is FALSE, generate the levels probabalistically
    cumprobs <- cumsum(probs/(sum(probs)))
    pick <- runif(size)
    choices <- outer(pick, cumprobs, FUN=`<=`) |>
      apply(1, function(x) min(which(x)))
    levels[choices]
  }
  #coin flips
  count_flips <- function(x, prob=0.5) {
    rbinom(size, size=x, prob)
  }
  #transformations
  binom <- function(logodds=NULL, prob=0.5, labels=NULL) {
    # 1 or 0 output with logistic input
    if (!is.null(logodds))  prob <- exp(logodds)/(1+exp(logodds))
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

    Res[[vnames[k]]] <- this
    #assign(vnames[k], this, envir=Res) # make it available for successive formulas.
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
  if (! report_hidden) rid <- grepl("^\\.", vnames)

  return(Res[, !rid])
}

#' @importFrom mosaic sample
#' @export
sample.dagsystem <- function(x, size, replace = FALSE, ...) {
  if (missing(size)) size=5
  dag_sample(x, size=size, ...)
}

#' @export
print.dagsystem <- function(x, ..., report_hidden=FALSE) {
  if (!report_hidden) {
    # find the hidden ones and suppress
    left_names <- lapply(x, FUN=function(x) all.vars(x[[2]]))
    rid <- which(grepl("^\\.", left_names))
    if (length(rid) > 0) x <- x[-rid]
  }
  cat(paste(unlist(lapply(x, FUN=capture.output)),
        collapse="\n"))
}




