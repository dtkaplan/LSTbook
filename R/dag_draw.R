#' Draw a DAG
#'
#' Make a simple drawing of a DAG.
#'
#' @details See the igraph package for more details.
#'
#' @param DAG The DAG to draw
#' @param \ldots Additional arguments to plot.igraph()
#'
#' @examples
#' dag_draw(dag03)
#' @export
dag_draw <- function(DAG, ...) {
  dots <- list(...)
  defaults <- list(vertex.size=40, vertex.color=NA,
                   vertex.shape="circle",
                   vertex.label.cex=2,
                   vertex.label.family="Courier",
                   vertex.frame.color=NA)
  if ("seed" %in% names(dots)) {
    set.seed(dots$seed)
    dots$seed <- NULL
  }
  # override defaults or add new arguments
  for (nm in names(dots)) {
    defaults[[nm]] <- dots[[nm]]
  }
  ig <- dag_to_igraph(DAG)

  do.call(plot, c(list(ig), defaults))
}


