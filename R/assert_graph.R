#' @title Assertion for mlr3pipelines Graph
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

#' @title Conversion to mlr3pipelines Graph
#'
#' @description
#' The argument is turned into a [`Graph`] if possible.
#' If `clone` is `TRUE`, a deep copy is made
#' if the incoming object is a [`Graph`] to ensure the resulting
#' object is a different reference from the incoming object.
#'
#' [`as_graph()`] is an S3 method and can therefore be implemented
#' by other packages that may add objects that can naturally be converted to [`Graph`]s.
#'
#' By default, [`as_graph()`] tries to
#' * apply [`gunion()`] to `x` if it is a `list`, which recursively applies [`as_graph()`] to all list elements first
#' * create a [`Graph`] with only one element if `x` is a [`PipeOp`] or can be converted to one using [`as_pipeop()`].
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
as_graph.default = function(x, clone = TRUE) {
  # different default than other methods for backwards compatibility
  # previously $add_pipeop() always cloned its input
  Graph$new()$add_pipeop(x, clone = clone)
}

#' @export
as_graph.list = function(x, clone = FALSE) {
  if (!clone && length(x) == 1L) {
    return(as_graph(x[[1L]], clone = clone))
  }
  gunion(x)  # gunion itself will convert individual members of x
}

#' @export
as_graph.Graph = function(x, clone = FALSE) {
  if (clone) {
    x = x$clone(deep = TRUE)
  }
  x
}

#' @title Assertion for mlr3pipelines PipeOp
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

#' @title Conversion to mlr3pipelines PipeOp
#'
#' @description
#' The argument is turned into a [`PipeOp`]
#' if possible.
#' If `clone` is `TRUE`, a deep copy is made
#' if the incoming object is a [`PipeOp`] to ensure the resulting
#' object is a different reference from the incoming object.
#'
#' [`as_pipeop()`] is an S3 method and can therefore be implemented by other packages
#' that may add objects that can naturally be converted to [`PipeOp`]s. Objects that
#' can be converted are for example [`Learner`][mlr3::Learner] (using [`PipeOpLearner`]) or
#' [`Filter`][mlr3filters::Filter] (using [`PipeOpFilter`]).
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
  stopf("%s can not be converted to PipeOp", deparse(substitute(x))[1L])
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

