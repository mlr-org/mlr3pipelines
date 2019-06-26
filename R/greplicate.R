#' @title Create Disjoint Graph Union of Copies of a Graph
#'
#' @description
#' Create a new [`Graph`] containing `n` copies of the input `Graph` / [`PipeOp`].
#' To avoid ID collisions, PipeOp IDs are suffixed with `_i`
#' where `i` ranges from 1 to `n`.
#'
#' @param graph [`Graph`] \cr
#'   Graph to replicate.
#' @param n `integer(1)`
#'   Number of copies to create.
#' @return [`Graph`] containing `n` copies of input `graph`.
#' @family Graph operators
#' @export
greplicate = function(graph, n) {
  graph = assert_graph(graph, coerce = TRUE)
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  x = map(seq_len(n), function(i) {
    g = graph$clone(deep = TRUE)
    ids = names2(g$pipeops)
    g$set_names(ids, sprintf("%s_%i", ids, i))
  })

  gunion(x)
}
