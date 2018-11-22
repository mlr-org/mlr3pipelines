########## Basic operator %>>% for 1-to-1, 1-to-n, n-to-1 ##########

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
`%>>%.list` = function(lhs, rhs) {
  `%>>%`(Graph$new(to_nodes_list(lhs)), rhs)
}

#' @export
`%>>%.Graph` = function(lhs, rhs) {

  # convert the rhs to list of Graphs
  rhs = sugar_rhs_wrap(rhs)
  rhs_list = extract_nodes_list(rhs)

  if(length(lhs$rhs) == 1 || length(rhs_list) == 1) {
    # 1-to-1, n-to-1 1-to-n
    lapply(lhs$rhs, function(x) x$set_next(rhs_list))
  } else {
    # FIXME - better error message
    stop("n-to-m not supported")
  }

  lhs
}

########## %>=>% n-to-n operator ##########
#' @export
`%>=>%` = function(lhs, rhs) {
  UseMethod("%>=>%")
}

#' @export
`%>=>%.PipeOp` = function(lhs, rhs) {
  `%>=>%`(GraphNode$new(lhs), rhs)
}

#' @export
`%>=>%.GraphNode` = function(lhs, rhs) {
  `%>=>%`(Graph$new(lhs), rhs)
}

#' @export
`%>=>%.list` = function(lhs, rhs) {
  `%>=>%`(Graph$new(to_nodes_list(lhs)), rhs)
}

#' @export
`%>=>%.Graph` = function(lhs, rhs) {

  # convert the rhs to list of Graphs
  rhs = sugar_rhs_wrap(rhs)
  rhs_list = extract_nodes_list(rhs)

  if(length(lhs$rhs) == length(rhs_list)) {
    # n-to-n
    mapply(function(x, y) x$set_next(y), lhs$rhs, rhs_list)
  } else {
    # FIXME - better error message
    stop("n-to-m not supported. %>=>% works only for n-to-n")
  }
  lhs
}

########## %>x>% n-to-m outer product ##########
#' @export
`%>x>%` = function(lhs, rhs) {
  UseMethod("%>x>%")
}

#' @export
`%>x>%.PipeOp` = function(lhs, rhs) {
  `%>x>%`(GraphNode$new(lhs), rhs)
}

#' @export
`%>x>%.GraphNode` = function(lhs, rhs) {
  `%>x>%`(Graph$new(lhs), rhs)
}

#' @export
`%>x>%.list` = function(lhs, rhs) {
  `%>x>%`(Graph$new(to_nodes_list(lhs)), rhs)
}

#' @export
`%>x>%.Graph` = function(lhs, rhs) {

  # convert the rhs to list of Graphs
  rhs = sugar_rhs_wrap(rhs)
  rhs_list = extract_nodes_list(rhs)
  lapply(lhs$rhs, function(x) x$set_next(rhs_list))
  lhs
}

####### Helper functions ######
extract_nodes_list = function(rhs) {
  unlist(lapply(rhs, function(x) x$source_nodes), recursive = FALSE)
}


to_nodes_list = function(x) {

  wrap = function(y) {
    checkmate::assert_multi_class(y, c("PipeOp", "GraphNode"))
    if(inherits(y, "PipeOp")) y = GraphNode$new(y)
    y
  }

  lapply(x, wrap)
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
