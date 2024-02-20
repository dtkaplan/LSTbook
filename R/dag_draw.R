#' Draw a DAG
#'
#' Make a simple drawing of a Directed Acyclic Graph as constructed by `datasim_make`.
#'
#' @details See the igraph package for more details.
#'
#' @param DAG The DAG to draw
#' @param \ldots Additional arguments to plot.igraph()
#' @param report_hidden logical. If `TRUE`, show the hidden nodes.
#'
#' @returns No return value. Called for graphics side-effects.
#'
#' @details By default, edges are not drawn to hidden nodes, that is, those whose
#' names begin with a dot. To show the hidden
#' nodes, use the argument `show_hidden=TRUE`.
#'
#' @examples
#' dag_draw(sim_03)
#' @export
dag_draw <- function(DAG, ..., report_hidden = FALSE) {
  dots <- list(...)
  if (requireNamespace("igraph", quietly = TRUE))
    for_layout <- igraph::layout_nicely
  else for_layout <- NULL
  defaults <- list(vertex.size = 40, vertex.color=NA,
                   vertex.shape = "circle",
                   vertex.label.cex = 2,
                   vertex.label.family = "Courier",
                   vertex.frame.color = NA,
                   layout = for_layout)
  if ("seed" %in% names(dots)) {
    set.seed(dots$seed)
    dots$seed <- NULL
  }
  # override defaults or add new arguments
  for (nm in names(dots)) {
    defaults[[nm]] <- dots[[nm]]
  }

  ig <- datasim_to_igraph(DAG, report_hidden = report_hidden)
  if (requireNamespace("graphics")) {
    oldpar <- graphics::par(mai = c(0,0,0,0)) # have the graph fill the frame
    on.exit(graphics::par(oldpar))
  }
  do.call(plot, c(list(ig), defaults))
}


