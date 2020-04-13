#' @title Create Disjoint Graph Union of Copies of a Graph
#'
#' @description
#' Create a new [`Graph`] containing `n` copies of the input `Graph` / [`PipeOp`].
#' To avoid ID collisions, PipeOp IDs are suffixed with `_i`
#' where `i` ranges from 1 to `n`.
#'
#' This function is deprecated and will be removed in the next version in favor
#' of using pipeline_greplicate / ppl("greplicate").
#'
#' @param graph [`Graph`] \cr
#'   Graph to replicate.
#' @param n `integer(1)`
#'   Number of copies to create.
#' @return [`Graph`] containing `n` copies of input `graph`.
#' @family Graph operators
#' @export
greplicate = function(graph, n) {
  warning('This function is deprecated and will be removed in the next version in favor of using ppl("greplicate").')
  pipeline_greplicate(graph, n)
}
