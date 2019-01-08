#' @title Union of graphs
#'
#' @description
#' Takes an arbitrary amount of Graphs, GraphNodes and PipeOps as inputs and joins
#' them by "stacking" them one over the other.
#' Returns the full graph.
#'
#' @param ... A list of [Graph], [GraphNode] or [PipeOp] \cr
#' List of elements with one of the types defined above.
#' @param .graphs list of [Graph]` \cr
#'   Graphs which are to be joined.
#' @return `[Graph]`
gunion = function(graphs) {
  graphs = map(graphs, ensure_graph)

  g = Graph$new()
  g$pipeops = unlist(map(graphs, "pipeops"), recursive = FALSE)
  assert_names(names(g$pipeops), type = "unique", .var.name = "ids of pipe operators")
  g$pipeops = map(g$pipeops, function(x) x$clone(deep = TRUE))
  g$channels = rbindlist(map(graphs, "channels"))
  g
}

