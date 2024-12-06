#' Construct and modify data simulations
#'
#' @details Simulations in `LSTbook` are first specified by providing the
#' code for each node (which can be written in terms of the values of other nodes). Once
#' constructed, data can be extracted from the simulation using `datasim_run(n)` or the
#' generic `take_sample(n)`.
#'
#' Each argument defines one node in the simulation. The argument syntax is unusual, using
#' *assignment*. For instance, an argument `y <- 3*x + rnorm(n)` defines a node named `y`. The R code
#' on the RHS of the assignment operator (that is, `3*x + rnorm(n)` in the example) will
#' be used by `datasim_run()` to generate the `y` variable when the simulation is run. Nodes
#' defined by previous arguments can be used in the code for later arguments.
#'
#' Helper functions such as `mix_with()`, `categorical()`, and several others can be used within
#' the node to perform complex operations.
#'
#' Remember to use *commas* to separate the arguments in the normal way.
#'
#' @param \ldots Descriptions of the nodes in the simulation, written in assignment form. See details.
#' @param sim The data simulation object to be modified.
#' @param report_hidden If `TRUE`, show the hidden nodes (nodes whose names
#' begin with a dot.)

#' @returns an object of class "datasim". Internally, this is a list of the R
#' assignment expressions used when running the simulation.
#'
#' @examples
#' Simple_sim <- datasim_make(x <- rnorm(n, sd=2), y <- 3*x + rnorm(n))
#' Simple_sim |> datasim_run(n = 5)
#' @rdname datasim_make
#' @export
datasim_make <- function(...) {
  steps <- enquos(..., .ignore_empty = "all")

  if (any(nchar(names(steps)) > 0))
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




#' @rdname datasim_make
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

#' @rdname datasim_make
#' @export
datasim_intervene <- function(sim, ...) {
  if (!inherits(sim, "datasim")) stop("Must provide a datasim object")
  new_steps <- enquos(..., .ignore_empty = "all")

  new_vnames <-
    lapply(new_steps, function(x) as.character(rlang::quo_get_expr(x)[[2]]))
  new_vcalls <- lapply(new_steps, function(x) rlang::quo_get_expr(x)[[3]])

  # replace the calls corresponding to any re-used names
  indices <- integer()
  for (i in seq_along(sim$names)) {
    for (j in seq_along(new_vnames)) {
      if (sim$names[[i]] == new_vnames[[j]]) {
        sim$calls[[i]] <- new_vcalls[[j]]
        indices <- c(indices, j)
      }
    }
  }

  # replace any steps already defined in the input datasim
  if (length(indices) > 0) {
    new_vcalls[indices] <- NULL
    new_vnames[indices] <- NULL
  }

  sim$names <- c(sim$names, new_vnames)
  sim$calls <- c(sim$calls, new_vcalls)

  put_in_order(sim, report_hidden = TRUE)

}



utils::globalVariables("block")
