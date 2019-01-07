########## Basic operator %>>% for 1-to-1, 1-to-n, n-to-1 ##########

#' @export
`%>>%` = function(lhs, rhs) {
  UseMethod("%>>%")
}

#' @export
`%>>%.PipeOp` = function(lhs, rhs) {
  graph = Graph$new(lhs$clone(deep = TRUE))
  `%>>%`(graph, rhs)
}

#' @export
`%>>%.GraphNode` = function(lhs, rhs) {
  `%>>%`(lhs$pipeop, rhs)
}

#' @export
`%>>%.list` = function(lhs, rhs) {
  Reduce(`%>>%`, c(lhs, list(rhs)))
}

#' @export
`%>>%.Graph` = function(lhs, rhs) {
  if (inherits(rhs, "list")) {
    rhs = Reduce(`%>>%`, rhs)
  }
  if (inherits(rhs, "GraphNode")) {
    rhs = rhs$pipeop
  }
  if (inherits(rhs, "PipeOp")) {
    graph = Graph$new(rhs$clone(deep = TRUE))
    rhs = graph
  }
  if (!inherits(rhs, "Graph")) {
    stop("%>>% can only be called with Graphs, GraphNodes, PipeOps, or lists of these.")
  }

  returngraph = Graph$new()
  if (length(lhs$out_channels) != length(rhs$in_channels)) {
    stop("Can only connect graphs if lhs has same number of out-channels as rhs has in-channels.")
  }
  returngraph$extend(lhs)
  returngraph$extend(rhs)

  for (idx in seq_along(lhs$out_channels)) {
    fromchan = lhs$out_channels[[idx]]
    tochan = rhs$in_channels[[idx]]
    newtochan = returngraph[[tochan$node$pipeop$id]]$in_channels[[tochan$channel_id]]
    returngraph[[fromchan$node$pipeop$id]]$next_node_channels[[fromchan$channel_id]] = newtochan
  }
  returngraph
}
