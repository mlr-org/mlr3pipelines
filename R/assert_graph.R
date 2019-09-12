#' @title Assertion for mlr3pipeline Graph
#'
#' @description
#' Function that checks that a given object is a `Graph` and
#' throws an error if not.
#' @param x (`any`) \cr
#'   Object to check.
#' @return [`Graph`] `invisible(x)`
#' @family Graph operators
#' @export
assert_graph = function(x) {
  assert_r6(x, "Graph")
}

#' @title Conversion to mlr3pipeline Graph
#'
#' @description
#' The object is turned into a `Graph` if possible.
#' If `clone` is `TRUE`, a deep copy is made
#' if the incoming object is a `Graph` to ensure the resulting
#' object is a different reference from the incoming object.
#'
#' @param x (`any`) \cr
#'   Object to convert.
#' @param clone (`logical(1)`) \cr
#'   Whether to return a (deep copied) clone if `x` is a Graph.
#' @return [`Graph`] `x` or a deep clone of it.
#' @family Graph operators
#' @export
as_graph = function(x, clone = FALSE) {
  UseMethod("as_graph")
}

#' @export
as_graph.default = function(x, clone = FALSE) {
  x = Graph$new()$add_pipeop(x)  # add_pipeop checks automatically for convertability
  if (clone) {
    x = x$clone(deep = TRUE)
  }
  x
}

#' @export
as_graph.list = function(x, clone = FALSE) {
  gunion(x)  # gunion itself will convert individual members of x
}

#' @export
as_graph.Graph = function(x, clone = FALSE) {
  if (clone) {
    x = x$clone(deep = TRUE)
  }
  x
}

#' @title Assertion for mlr3pipeline PipeOp
#'
#' @description
#' Function that checks that a given object is a `PipeOp` and
#' throws an error if not.
#' @param x (`any`) \cr
#'   Object to check.
#' @return [`PipeOp`] `invisible(x)`
#' @family Graph operators
#' @export
assert_pipeop = function(x) {
  assert_r6(x, "PipeOp")
  invisible(x)
}

#' @title Conversion to mlr3pipeline PipeOp
#'
#' @description
#' The object is turned into a `PipeOp`
#' if possible.
#' If `clone` is `TRUE`, a deep copy is made
#' if the incoming object is a `PipeOp` to ensure the resulting
#' object is a different reference from the incoming object.
#'
#' @param x (`any`) \cr
#'   Object to convert.
#' @param clone (`logical(1)`) \cr
#'   Whether to return a (deep copied) clone if `x` is a PipeOp.
#' @return [`PipeOp`] `x` or a deep clone of it.
#' @family Graph operators
#' @export
as_pipeop = function(x, clone = FALSE) {
  UseMethod("as_pipeop")
}

#' @export
as_pipeop.default = function(x, clone = FALSE) {
  stopf("%s can not be converted to PipeOp", deparse(substitute(x))[1])
}

#' @export
as_pipeop.PipeOp = function(x, clone = FALSE) {
  if (clone) {
    x = x$clone(deep = TRUE)
  }
  x
}

#' @export
as_pipeop.Learner = function(x, clone = FALSE) {
  PipeOpLearner$new(x)
}

#' @export
as_pipeop.Filter = function(x, clone = FALSE) {
  PipeOpFilter$new(x)
}

