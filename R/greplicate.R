
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




