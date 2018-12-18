# Union of graphs
# takes an arbitrary amount of Graphs, GraphNodes and PipeOps as inputs and joins them by "stacking" them one over the other
# Returns a single Graph
gunion = function(...) {
  graphs = list(...)
  graphs = map_if(graphs, function(x) inherits(x, "PipeOp"), function(x) GraphNode$new(x))
  graphs = map_if(graphs, function(x) inherits(x, "GraphNode"), function(x) Graph$new(x))
  assertList(graphs, types = "Graph")
  start_nodes = unlist(map(graphs, function(x) x$lhs), recursive = FALSE)
  Graph$new(start_nodes)
}

if (FALSE) {
  g1 = PipeOpPCA$new() %>>% PipeOpScaler$new()
  g2 = PipeOpPCA$new(id = "pca2") %>>% PipeOpFeatureTransform$new()
  g3 = PipeOpFeatureTransform$new(id = "blub") %>>% PipeOpScaler$new(id = "foo")
  g4 = gunion(g1, g2, g3)

  g5 = PipeOpPCA$new() %>>% PipeOpScaler$new()
  g6 = PipeOpPCA$new(id = "pca2") %>>% PipeOpFeatureTransform$new()
  g7 = PipeOpFeatureTransform$new(id = "blub") %>>% PipeOpScaler$new(id = "foo")
  g8 = gunion(g5, g6)
  g9 = gunion(g8, g7)

  g10 = PipeOpPCA$new() %>>% PipeOpScaler$new()
  g11 = PipeOpPCA$new("asdf")
  g12 = GraphNode$new(PipeOpPCA$new("asdf2"))
  g13 = gunion(g10, g11, g12)

  g14 = gunion(PipeOpPCA$new(), PipeOpScaler$new())
}


#replicate a graph and joins it by a union
# takes a Graph and integer n
# returns a Graph
#' @export
greplicate = function(graph, n) {
  UseMethod("greplicate")
}

#' @export
greplicate.PipeOp = function(graph, n) {
  greplicate(GraphNode$new(graph), n)
}

#' @export
greplicate.GraphNode = function(graph, n) {
  greplicate(Graph$new(graph), n)
}

#' @export
greplicate.Graph = function(graph, n) {
  graphs = map(seq_len(n), function(i) {
    g_new = graph$clone(deep = TRUE)
    #FIXME: This would be nicer with a purrr modify
    for (id in graph$ids) {
      node = g_new$find_by_id(id)
      node$pipeop$id = paste(id, i, sep = "_")
    }
    return(g_new)
  })
  do.call(gunion, graphs)
}



if (FALSE) {
  load_all()
  graph = PipeOpPCA$new() %>>% PipeOpScaler$new()
  g2 = greplicate(graph, 1) #Does not work yet
  g2$plot()
  greplicate(PipeOpPCA$new(), 5)
  greplicate(GraphNode$new(PipeOpPCA$new()), 5)
}
