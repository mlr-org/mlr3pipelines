#' @export
`%>>%` = function(lhs, rhs) {
  UseMethod("%>>%")
}

#' @export
`%>>%.PipeOp` = function(lhs, rhs) {
  `%>>%`(GraphNode$new(lhs), rhs)
}

#' @export
`%>>%.GraphNode` = function(lhs, rhs) {
  `%>>%`(Graph$new(lhs), rhs)
}



#' @export
`%>>%.Graph` = function(lhs, rhs) {

  # convert the rhs to list of Graphs
  rhs = sugar_rhs_wrap(rhs)
  rhs_list = lapply(rhs, function(x) x$source_node)

  if(length(lhs$rhs) == 1 || length(rhs_list) == 1) {
    # 1-to-1, n-to-1 1-to-n
    lapply(lhs$rhs, function(x) x$set_next(rhs_list))
  } else {
    # FIXME - better error message
    stop("n-to-m not supported")
  }

  lhs
}

#' Automatically transform PipeOp, list of PipeOps, GraphNode, Graph to list of Graphs.
#'
#' @param x
#'
#' @return
#'
#' If \code{x} is a Graph the function returns the list(x).
#'
#' @noRd
#'
sugar_rhs_wrap = function(x) {

  wrap = function(y) {
    checkmate::assert_multi_class(y, c("PipeOp", "GraphNode", "Graph"))
    if(inherits(y, "PipeOp")) {
      y = Graph$new(GraphNode$new(y))
    } else if(inherits(y, "GraphNode")) {
      y = Graph$new(y)
    }
    y
  }

  if(inherits(x, "PipeOp") || inherits(x, "GraphNode") || inherits(x, "Graph")) {
    x = list(wrap(x))
  } else if(is.list(x)) {
    x = lapply(x, wrap)
  }
  x
}
