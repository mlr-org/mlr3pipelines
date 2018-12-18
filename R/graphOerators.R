# Union of graphs
# takes multiple graphs and joins them "stacking" them one over the other
# Outputs a Graph
gunion = function(...) {
  graphs = list(...)
  assertList(graphs, types = "Graph")
  start_nodes = unlist(lapply(graphs, function(x) x$lhs), recursive = FALSE)
  Graph$new(start_nodes)
}

greplicate = function(graph, n) {
  graphs = replicate(n, expr = graph$clone(deep = TRUE), simplify = FALSE)
  Graph$new(unlist(lapply(graphs, function(g) g$lhs)))
  updateIds = function(graph, id_append) {
    for (id in graph$ids) {
      graph$find_by_id(id)$id = paste0(graph$find_by_id(id)$id, id_append)
    }
  }
}

if (FALSE) {
  g1 = PipeOpPCA$new() %>>% PipeOpScaler$new()
  updateIds(g1, "bla")
  ng = greplicate(g1, 10)
  ng$lhs
  ng$rhs
}

