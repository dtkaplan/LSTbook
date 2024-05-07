#' Helpers for specifying nodes in simulations
#'
#' @details `datasim_make()` constructs a simulation
#' which can then be run with `datasim_run()`. Each argument to
#' `datasim_make()` specifies one node of the simulation using an
#' assignment-like syntax such as `y <- 3*x + 2 + rnorm(n)`. The datasim
#' helpers documented here are for use on the right-hand side of the specification
#' of a node. They simplify potentially complex operations such as blocking, creation
#' of random categorical methods, translation from categorical to numerical values, etc.

#' @param n The symbol standing for the number of rows in the data frame to be generated
#' by `datasim_run()`. Just use `n` as a symbol; don't assign it a value. (That will
#' be done by `datasim_run()`.)
#' @param exact Logical flag. If `TRUE`, be scrupulous in dividing up the number of factors
#' @param logodds Numerical vector used to generate bernouilli trials. Can be any real number.
#' @param prob An alternative to `logodds`. Values must be in `[0,1]`.
#' @param labels Character vector: names for categorical levels, also used to
#' replace 0 and 1 in bernouilli()
#' @param block_var Which variable to use for blocking
#' @param \ldots named arguments giving relative probability of each category
#'
#' @returns A numerical or categorical vector which will be assembled into
#' a data frame by `datasim_run()`
#'
#' @examples
#' Demo <- datasim_make(
#'   g <- categorical(n, a=2, b=1, c=0.5),
#'   x <- cat2value(g, a=-1.7, b=0.1, c=1.2),
#'   y <- bernoulli(logodds = x, labels=c("no", "yes")),
#'   z <- random_levels(n, k=4),
#'   w <- mix_with(x, noise=rnorm(n), R2=0.75, var=1),
#'   treatment <- block_by(w),
#'   dice <- each(rnorm(1, sd = abs(w)))
#' )
#'
#' @rdname datasim_helpers
#' @export
categorical <- function(n=5, ..., exact = TRUE) {
  # specify levels either as a list or using ...
  dots <- list(...)

  # handle case where some or all of dots do not have a numeric value.

  probs <- suppressWarnings(as.numeric(unlist(dots)))
  if (all(is.na(probs))) {
    # handle case where dots is a list of character strings
    levels <- unlist(dots)
    probs <- rep(1, length(levels))
  } else if (any(is.na(probs))) {
    # if some of dots are character strings, but not others, balk
    stop("Give relative probability of all levels, not just some of them.")
  } else {
    # all of the dots are numeric
    levels <- names(unlist(dots))
  }

  # Convert to normalized probabilities
  probs <- probs / sum(probs)

  # generate the levels
  if (exact) { # table of counts should be as exactly as possible matched to probs.
    if (length(unique(probs)) == 1) # all the same
      return(base::sample(rep_len(levels, length.out=n)))
    else {
      counts <- round(probs*n)
      res <- base::sample(rep.int(levels, times=counts))
      # deal with round-off in the counts
      if (length(res) > n) res <- res[1:n]
      else if (length(res) != n) {
        res <- c(res,
                 categorical(n = n-length(res), ...,
                             exact=FALSE))
      }
      return(res)
    }
  } else {
    # When <exact> is FALSE, generate the levels probabalistically
    cumprobs <- cumsum(probs/(sum(probs)))
    pick <- stats::runif(n)
    choices <- outer(pick, cumprobs, FUN=`<=`) |>
      apply(1, function(x) min(which(x)))
    return(levels[choices])
  }
}

#' @param variable a categorical variable
#' @param \ldots assignments of values to the names in `variable`
#' @rdname datasim_helpers
#' @export
cat2value <- function(variable, ...) {
  values <- unlist(list(...))
  levels <- names(values)
  missing <- setdiff(variable, levels)
  values[missing] <- NA

  as.vector(values[variable])
}


#' @rdname datasim_helpers
#' @export
bernoulli <- function(n=0, logodds=NULL, prob=0.5, labels=NULL) {
  if (length(logodds) > 0) n <- length(logodds)
  if (length(prob) > 1) n <- length(prob)
  else if (length(prob) == 1L) {
    if (n == 0) stop("Must specify <n=> in bernoulli() unless <logodds=> or <prob=> is used.")
    prob <- rep(prob, n)
  }
  # 1 or 0 output with logistic input
  if (!is.null(logodds))  prob <- exp(logodds) / (1 + exp(logodds))
  yesno <- as.numeric(stats::runif(n) < prob)
  if (!is.null(labels) && length(labels) == 2)
    yesno <- labels[yesno + 1]

  yesno
}

#' Mix two variables together. The output will have the specified R-squared
#' with var1 and variance one.
#' @param signal The part of the mixture that will be correlated with
#' the output.
#' @param noise The rest of the mixture. This will be **uncorrelated**
#' with the output only if you specify it as pure noise.
#' @param R2 The target R-squared.
#' @param var The target variance.
#' @param exact if `TRUE`, make R-squared or the target variance exactly as specified.
#'
#' @details The target R-squared and variance will be achieved only
#' if `exact=TRUE` or the sample size goes to infinity.
#' @importFrom stats resid rnorm sd
#' @rdname datasim_helpers
#' @export
mix_with <- function(signal, noise = NULL, R2 = 0.5, var = 1, exact=FALSE) {
  if (R2 < 0 || R2 > 1) stop("R2 must be between zero and one.")
  if (var < 0) {
    var <- abs(var)
    warning("Negative variance specified with `var`. Taking absolute value")
  }
  if (is.null(noise)) noise <- rnorm(length(signal))
  if(exact) {
    noise <- resid(lm(noise ~ signal))
  }

  sd_signal <- sd(signal, na.rm = TRUE)
  sd_noise  <- sd(noise, na.rm = TRUE)
  mix <- sqrt(R2) * signal/sd_signal + sqrt(1-R2)*noise/sd_noise

  mix * sqrt(var) / sd(mix)
}
#' Evaluate an expression separately for each case
#' @param ex an expression potentially involving other variables.
#' @rdname datasim_helpers
#' @export
each <- function(ex) {
  ex <- substitute(ex)
  f <- function(env) {
    eval(ex, envir = env)
  }
  class(f) <- c(class(f), "each-object")

  f # returns a function which, when evaluated, gives a
  # value for one row of the data frame based on the values already
  # in that row
}


#' @param levels Character vector giving names to the blocking levels
#' @param show_block Logical. If `TRUE`, put the block number in the output.
#' @rdname datasim_helpers
#' @export
block_by <- function(block_var,
                     levels = c("treatment", "control"),
                     show_block=FALSE) {
  orig <- seq_along(block_var)
  inds <- order(block_var)
  block_var <- block_var[inds]
  # divide into even-sized groups
  Tmp <- tibble::tibble(orig = orig[inds],
                        block = (seq_along(block_var) - 1) %/% length(levels)) |>
    mutate(out = levels[rank(stats::runif(n()))], .by = block) |>
    arrange(orig)

  if (show_block) Tmp$block
  else Tmp$out
}

#' @rdname datasim_helpers
#' @param k Number of distinct levels
#' @param replace if `TRUE`, use resampling on the set of k levels
#' @export
random_levels <- function(n, k = NULL, replace = FALSE) {
  if (is.null(k)) stop("Must specify k as number of distinct levels.")
  rlevels <- stringi::stri_rand_strings(k, 6, pattern = "[A-Za-z0-9]")
  if (replace) {
    take_sample(rlevels, n, replace = TRUE)
  } else {
    take_sample(rep(rlevels, length.out = n))
  }
}
