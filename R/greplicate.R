
#' @title Create Disjoint Graph Union of Copies of a Graph
#'
#' Create a new graph containing `n` copies of the input graph.
#' To avoid ID collisions, PipeOp IDs are suffixed with `_i`
#' where `i` ranges from 1 to `n`.
#'
#' @param graph [`Graph`] \cr
#'   Graph to replicate.
#' @param n `integer(1)`
#'   Number of copies to create.
#' @return [`Graph`] containing `n` copies of input `graph`.
#' @export
greplicate = function(graph, n) {
  graph = assert_graph(graph, coerce = TRUE)
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  x = map(seq_len(n), function(i) {
    g = graph$clone(deep = TRUE)
    ids = names(g$pipeops)
    g$set_names(ids, sprintf("%s_%03i", ids, i))
  })

  gunion(x)
}
