# Union of graphs
# takes an arbitrary amount of Graphs, GraphNodes and PipeOps as inputs and joins them by "stacking" them one over the other
# Returns a single Graph
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

#replicate a graph and joins it by a union
# takes a Graph and integer(1) n
# n may also be a character: a vector of postfixes to use.
# returns a Graph
#' @export
greplicate = function(graph, n) {
  UseMethod("greplicate")
}

#' @export
greplicate.PipeOp = function(graph, n) {
  g = Graph$new()
  g$add_node(graph)
  greplicate(g, n)
}

#' @export
greplicate.Graph = function(graph, n) {
  if (is.numeric(n)) {
    n = as.character(seq_len(n))
  }
  gunion(.graphs = sapply(n, function(.) graph, simplify = FALSE))
}



