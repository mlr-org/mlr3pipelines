#' @title Union of graphs
#'
#' @description
#' Takes an arbitrary amount of [Graph]s and/or [PipeOp]s as inputs and joins
#' them by "stacking" them one over the other.
#' Returns the full graph.
#'
#' @param graphs A list of [Graph] or [PipeOp] \cr
#'   List of elements with one of the types defined above, which are the
#'   graphs to be joined.
#' @return `[Graph]`
#' @export
gunion = function(graphs) {
  assert_list(graphs)
  graphs = map(graphs, ensure_graph)

  g = Graph$new()
  g$pipeops = unlist(map(graphs, "pipeops"), recursive = FALSE)
  assert_names(names(g$pipeops), type = "unique", .var.name = "ids of pipe operators")
  g$pipeops = map(g$pipeops, function(x) x$clone(deep = TRUE))
  g$edges = rbindlist(map(graphs, "edges"))
  g
}

