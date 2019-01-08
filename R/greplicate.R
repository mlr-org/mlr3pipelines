
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
  greplicate(ensure_graph(graph), n)
  greplicate(g, n)
}

#' @export
greplicate.Graph = function(graph, n) {
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  graphs = replicate(n, graph$clone(deep = TRUE))
  gunion(graphs)
}




