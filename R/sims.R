#' Facility to make and run data simulations
#'
#' @param sim A data simulation as made by `datasim_make()`
#' @param n The sample size
#' @param exact Logical flag. If `TRUE`, be scrupulous in dividing up the number of factors
#' @param logodds Numerical vector used to generate bernouilli trials. Can be any real number.
#' @param prob An alternative to `logodds`. Values must be in `[0,1]`.
#' @param labels Character vector: names for categorical levels, also used to
#' replace 0 and 1 in bernouilli()
#' @param block_var Which variable to use for blocking
#' @param report_hidden In graphing a dag, show the hidden nodes (nodes whose name begins with `.`)
#' @param seed Integer to use as a random seed (optional) for reproducibility
#'
#' @rdname datasim
#' @export
datasim_make <- function(...) {
  steps <- enquos(..., .ignore_empty = "all")

  if (any(nchar(names(steps[[1]])) > 0))
    stop("Use the storage arrow `<-` (not the equation sign `=`) in arguments to datasim_make().")
  # Pull out the variable names from the left-hand side of <-
  vnames <- lapply(steps, function(x) rlang::quo_get_expr(x)[[2]])
  # ... and the simulation rules from the right-hand side.
  vcalls <- lapply(steps, function(x) rlang::quo_get_expr(x)[[3]])


  # Can't use `n` as a variable because it would be confused with `n` as size placeholder.
  if ("n" %in% unlist(vnames)) stop("Can't use `n` as a simulation variable name.")

  sim <- list(names = vnames, calls = vcalls)
  class(sim) <- c("list", "datasim")

  put_in_order(sim)
}

put_in_order <- function(sim, report_hidden=TRUE) {
  # Put the nodes in topological order so that every call refers
  # only to nodes further down the list.
  # the "hidden" nodes are included when putting them in order
  if (requireNamespace("igraph", quietly = TRUE)) {
    # Remove dependency on igraph for WebR compatibility
    new_order <- datasim_to_igraph(sim, report_hidden=TRUE) |> igraph::topo_sort()
    sim$names <- sim$names[new_order]
    sim$calls <- sim$calls[new_order]
  }

  sim
}
#' @export
print.datasim <- function(x, ..., report_hidden = FALSE) {
  vnames <- lapply(x$names, as.character) |> unlist()
  vcalls <- x$calls
  # Hide the hidden ones
  if (!report_hidden) {
    rid <- grepl("^\\.", vnames)
    vnames <- vnames[!rid]
    vcalls <- vcalls[!rid]
  }

  RHS <- lapply(vcalls, deparse, width.cutoff = 500) |> unlist()
  components <- glue::glue("[{seq_along(vnames)}] {vnames} <- {RHS}")

  cat(paste0(
    "Simulation object\n------------\n",
    paste(components, collapse="\n")))

}
#' @param report_hidden logical. If `TRUE`, show the values of hidden variables (that is, variables whose name
#' begins with a dot)
#' @rdname datasim
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


#' @rdname datasim
#' @param \ldots named arguments giving relative probability of each category
#'
#' @export
categorical <- function(n=5, ..., exact = TRUE) {
  # specify levels either as a list in <levels> or using ...
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
#' @rdname datasim
#' @export
cat2value <- function(variable, ...) {
  values <- unlist(list(...))
  levels <- names(values)
  missing <- setdiff(variable, levels)
  values[missing] <- NA

  as.vector(values[variable])
}


#' @rdname datasim
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
#' @param input The part of the mixture that will be correlated with
#' the output.
#' @param noise The rest of the mixture. This will be **uncorrelated**
#' with the output only if you specify it as pure noise.
#' @param R2 The target R-squared.
#' @param var The target variance.
#'
#' @details The target R-squared and variance will be achieved only
#' if `exact=TRUE` or the sample size goes to infinity.

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
#' @rdname datasim
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

#' @rdname datasim
#' @export
datasim_to_igraph <- function(sim, report_hidden=FALSE) {
  nnames <- sim$names |> unlist()
  # Get rid of the hidden ones
  if (!report_hidden) nnames[grepl("^\\.", nnames)] <- " "

  edges <- numeric(0)
  for (k in 1:length(nnames)) {
    # loop over all the nodes
    from_names <- all.names(sim$calls[[k]]) |> setdiff("n")
    from_nums <- which(nnames %in% from_names)
    if (length(from_nums) > 0 ) {
      new_edges <- c(rbind(rbind(from_nums), k))
      edges <- c(edges, new_edges)
    }

  }

  if (requireNamespace("igraph", quietly = TRUE)) {
    g <- igraph::make_empty_graph(n=length(nnames), directed=TRUE) %>%
      igraph::add_edges(edges) %>%
      igraph::set_vertex_attr("label", value=nnames)


    return(g)
  } else {
    stop("Package `{igraph} not available to draw datasim.")
  }
}

#' @rdname datasim
#' @export
datasim_intervene <- function(sim, ...) {
  if (!inherits(sim, "datasim")) stop("Must provide a datasim object")
  new_steps <- enquos(..., .ignore_empty = "all")

  new_vnames <-
    lapply(new_steps, function(x) as.character(rlang::quo_get_expr(x)[[2]]))
  new_vcalls <- lapply(new_steps, function(x) rlang::quo_get_expr(x)[[3]])
  # replace the calls corresponding to any re-used names
  reused <- which(as.character(sim$names) %in% as.character(new_vnames))
  indices <- which(as.character(new_vnames) %in% as.character(sim$names))
  # replace any steps already defined in the input datasim
  if (length(indices) > 0) {
    sim$calls[reused] <- new_vcalls[indices]
    new_vcalls[indices] <- NULL
    new_vnames[indices] <- NULL
  }

  sim$names <- c(sim$names, new_vnames)
  sim$calls <- c(sim$calls, new_vcalls)

  put_in_order(sim, report_hidden = TRUE)

}

#' @rdname datasim
#' @param k Number of distinct levels
#' @param replace if `TRUE`, use resampling on the set of k levels
#' @export
random_levels <- function(n, k = NULL, replace = FALSE) {
  if (is.null(k)) stop("Must specify k as number of distinct levels.")
  rlevels <- stringi::stri_rand_strings(k, 6, pattern = "[A-Za-z0-9]")
  if (replace) {
    sample(rlevels, n, replace = TRUE)
  } else {
    sample(rep(rlevels, length.out = n))
  }
}

utils::globalVariables("block")
