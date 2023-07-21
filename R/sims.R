#' Facility to make and run data simulations
#'
#' @rdname datasim
#' @export
datasim_make <- function(...) {
  steps <- enquos(..., .ignore_empty = "all")
  vnames <- lapply(steps, function(x) rlang::quo_get_expr(x)[[2]])
  vcalls <- lapply(steps, function(x) rlang::quo_get_expr(x)[[3]])


  sources <- lapply(vcalls, function(x) setdiff(all.vars(x), "n"))

  sim <- list(names = vnames, calls = vcalls)

  # Put the nodes in topological order so that every call refers
  # only to nodes further down the list.

  new_order <- datasim_to_igraph(sim) |> igraph::topo_sort()
  sim$names <- sim$names[new_order]
  sim$calls <- sim$calls[new_order]

  class(sim) <- c("list", "datasim")

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

  RHS <- lapply(vcalls, deparse) |> unlist()
  components <- glue::glue("[{seq_along(vnames)}] {vnames} <- {RHS}")

  cat(paste0(
    "Simulation object\n------------\n",
    paste(components, collapse="\n")))

}
#' @rdname datasim
#' @export
datasim_run <- function(sim, n=5, seed=NULL) {
  # set random number generator seed, if called for
  if (!is.null(seed)) set.seed(seed)
  # custom functions
  exo <- rnorm

  # create stub for the output
  values <- vector("list", length(sim$names))
  names(values) <- as.character(sim$names)
  values[["n"]] <- n # special variable

  # construct the values
  for (k in seq_along(sim$names)) {
    values[[k]] <- eval(sim$calls[[k]], values)
  }

  # Get rid of unwanted names, such as those starting with a dot (".")
  values$n <- NULL
  rid <- grepl("^\\.", names(values))
  if (any(rid)) values[[rid]] <- NULL

  # return a data frame
  as_tibble(values)
}

#' @importFrom mosaic sample
#' @export
sample.datasim <- function(x, size, replace = FALSE, ...) {
  if (missing(size)) size=5
  datasim_run(x, size=size, ...)
}

#' @rdname datasim
#' @export
categorical <- function(n=5, levels = NULL, ..., exact = TRUE) {
  # specify levels either as a list in <levels> or using ...
  if (!is.null(levels)) dots <- levels
  else dots <- list(...)

  levels <- names(dots)
  if (any(nchar(levels) == 0)) stop("All levels must be given names.")

  # Get the weight associated with each level
  probs <- abs(as.numeric(unlist(dots)))
  # Convert to cumulative probabilities
  probs <- probs / sum(probs)

  # generate the levels
  if (exact) { # table of counts should be as exactly as possible matched to probs.
    if (length(unique(probs)) == 1) # all the same
      return(sample(rep_len(levels, length.out=n)))
    else {
      counts <- round(probs*n)
      res <- sample(rep.int(levels, times=counts))
      if (length(res) != n) {
        res <- c(res,
                 categorical(n = n-length(res),
                             levels = dots, exact=FALSE))
      }
      return(res)
    }
  } else {
    # When <exact> is FALSE, generate the levels probabilistically
    cumprobs <- cumsum(probs/(sum(probs)))
    pick <- runif(n)
    choices <- outer(pick, cumprobs, FUN=`<=`) |>
      apply(1, function(x) min(which(x)))
    return(levels[choices])
  }
}

#' @rdname datasim
#' @export
bernoulli <- function(logodds=NULL, prob=0.5, labels=NULL, n=0) {
  if (length(logodds) > 0) n <- length(logodds)
  else if (length(prob) == 1L) {
    if (n == 0) stop("Must specify <n=> in bernoulli() unless <logodds=> or <prob=> is used.")
    prob <- rep(prob, n)
  }
  # 1 or 0 output with logistic input
  if (!is.null(logodds))  prob <- exp(logodds)/(1+exp(logodds))
  yesno <- as.numeric(runif(n) < prob)
  if (!is.null(labels) && length(labels)==2)
    yesno <- labels[yesno+1]

  yesno
}

#' @rdname datasim
#' @export
block_by <- function(block_var, levels=c("treatment", "control")) {
  orig <- seq_along(block_var)
  inds <- order(block_var)
  block_var <- block_var[inds]
  if (is.numeric(block_var)) {
    # divide into even-sized groups
    block_var <- (seq_along(block_var)-1) %/% length(levels)
  }
  orig <- orig[inds]
  out <- rep_len(levels, length.out=length(block_var))
  out <- mosaic::sample(out, groups=block_var) # randomize the order within the blocks
  back_inds <- order(orig)
  out[back_inds]
}


#' @rdname datasim
#' @export
datasim_to_igraph <- function(sim, show_hidden=FALSE) {
  nnames <- sim$names |> unlist()
  # Get rid of the hidden ones
  if (!show_hidden) nnames[grepl("^\\.", nnames)] <- " "

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

  g <- igraph::make_empty_graph(n=length(nnames), directed=TRUE) %>%
    igraph::add_edges(edges) %>%
    igraph::set_vertex_attr("label", value=nnames)


  g
}



categorical(12, a=1, b=2, c=3, d=0.1, exact=TRUE)
bernoulli(10, labels=c("yes", "no")) |> table()
block_by(categorical(10, a=1, b=2))

sim <- datasim_make( group <- categorical(n, a=1, b=2),
                 treat <- block_by(group))
sim2 <- datasim_make( group <- exo(n),
                  treat <- block_by(group))
sim3 <- datasim_make( ,
                  block_by(group) -> treat,
                  group <- exo(n),)
