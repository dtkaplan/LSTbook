#' helper functions for transforming and checking DAGs
#'
dag_to_igraph <- function(DAG) {
  nnames <- node_names(DAG)


  edges <- numeric(0)
  for (k in 1:length(DAG)) {
    # loop over all the nodes
    from_names <- all.names(DAG[[k]][[3]])
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

dag_sort <- function(DAG) {
  new_order <- dag_to_igraph(DAG) %>% igraph::topo_sort()

  res <- DAG[new_order]
  class(res) <- c("dagsystem")

  res
}

node_names <- function(DAG) {
  res <- unlist(
    lapply(DAG, function(x) all.vars(x[[2]]))
  )
  if (length(unique(res)) != length(res))
    stop("Duplicate name in DAG")
  res
}

source_nodes <- function(DAG) {
  input_names <- unlist(
    lapply(DAG, function(x) all.vars(x[[3]]))
  )

  unique(input_names)
}

# make sure all nodes used on the right side of formulas
# appear on the left-hand side of one of the formulas.
dag_check <- function(DAG) {
  all(source_nodes(DAG) %in% node_names(DAG))
}


