#' @title Union of graphs
#' 
#' @description
#' Takes an arbitrary amount of Graphs, GraphNodes and PipeOps as inputs and joins
#' them by "stacking" them one over the other.
#' Returns the full graph.
#' @param `...` `list of `[Graph]`, `[GraphNode]` or `[PipeOp]`` \cr
#' List of elements with one of the types defined above.
#' @param `.graphs` `list of [Graph]` \cr
#' @return `[Graph]`
gunion = function(..., .graphs = NULL) {
  graphs = c(list(...), .graphs)
  resultgraph = Graph$new()
  for (idx in seq_along(graphs)) {
    g = graphs[[idx]]
    n = names2(graphs)[idx]
    if (inherits(g, "PipeOp")) {
      g = g$clone(deep = TRUE)
      if (!is.na(n)) {
        g$id = paste(g$id, n, sep = "_")
      }
      resultgraph$add_node(g)
    } else if (inherits(g, "Graph")) {
      intermediate = Graph$new(g)
      if (!is.na(n)) {
        for (nodeid in names(intermediate$node_list)) {
          intermediate[[nodeid]]$pipeop$id = paste(nodeid, n, sep = "_")
        }
        intermediate$update_ids()
      }
      resultgraph$extend(intermediate)
    } else {
      stop("Given element was not a Graph or PipeOp")
    }
  }
  resultgraph
}

