#' @title Disjoint Union of Graphs
#'
#' @description
#' Takes an arbitrary amount of [`Graph`]s and/or [`PipeOp`]s as inputs and joins
#' them in a new `Graph`.
#'
#' The `PipeOp`s of the input `Graph`s are not joined with new edges across
#' `Graph`s, so if `length(graphs) > 1` the resulting `Graph` will be disconnected.
#'
#' @param graphs `list` of ([`Graph`] | [`PipeOp`]) \cr
#'   List of elements with one of the types defined above, which are the
#'   graphs to be joined.
#' @return [`Graph`] the resulting `Graph`.
#'
#' @family Graph operators
#' @export
gunion = function(graphs) {
  assert_list(graphs)
  graphs = Filter(function(x) length(x$pipeops), map(graphs, assert_graph, coerce = TRUE))


  g = Graph$new()
  if (length(graphs)) {
    g$pipeops = unlist(map(graphs, "pipeops"), recursive = FALSE)
    assert_names(names(g$pipeops), type = "unique", .var.name = "ids of pipe operators")
    g$pipeops = map(g$pipeops, function(x) x$clone(deep = TRUE))
    g$edges = rbindlist(map(graphs, "edges"), use.names = TRUE)
  }
  g
}

