
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
}

# FIXME: unify we dont need s3

#' @export
greplicate.Graph = function(graph, n) {
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  x = map(seq_len(n), function(i) {
    g = graph$clone(deep = TRUE)
    ids = names(g$pipeops)
    g$set_names(ids, sprintf("%s_%03i", ids, i))
  })

  gunion(x)
}
