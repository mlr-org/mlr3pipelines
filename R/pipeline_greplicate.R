#' @include mlr_graphs.R

#' @title Create Disjoint Graph Union of Copies of a Graph
#'
#' @description
#' Create a new [`Graph`] containing `n` copies of the input [`Graph`] / [`PipeOp`]. To avoid ID
#' collisions, PipeOp IDs are suffixed with `_i` where `i` ranges from 1 to `n`.
#'
#' All input arguments are cloned and have no references in common with the returned [`Graph`].
#'
#' @param graph [`Graph`] \cr
#'   Graph to replicate.
#' @param n `integer(1)`
#'   Number of copies to create.
#' @return [`Graph`] containing `n` copies of input `graph`.
#' @family Graph operators
#' @export
#' @examples
#' library("mlr3")
#'
#' po_pca = po("pca")
#' pipeline_greplicate(po_pca, n = 2)
pipeline_greplicate = function(graph, n) {
  graph = as_graph(graph)
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  x = map(seq_len(n), function(i) {
    g = graph$clone(deep = TRUE)
    g$update_ids(postfix = paste0("_", i))
  })

  gunion(x, in_place = TRUE)
}

mlr_graphs$add("greplicate", pipeline_greplicate)

