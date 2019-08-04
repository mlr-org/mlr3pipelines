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
  invisible(x)
}

#' @title Conversion to mlr3pipeline Graph
#'
#' @description
#' The object is turned into a `Graph` if possible.
#' If `deep_copy` is `TRUE`, a deep copy is made
#' if the incoming object is a `Graph` to ensure the resulting
#' object is a different reference from the incoming object.
#'
#' @param x (`any`) \cr
#'   Object to convert.
#' @param deep_copy (`logical(1)`) \cr
#'   Whether to return a deep copy if `x` is a Graph.
#' @return [`Graph`] `x` or a deep clone of it.
#' @family Graph operators
#' @export
as_graph = function(x, deep_copy = FALSE) {
  UseMethod("as_graph")
}

as_graph.default = function(x, deep_copy = FALSE) {
  x = Graph$new()$add_pipeop(x)  # add_pipeop checks automatically for convertability
  if (deep_copy) {
    x = x$clone(deep = TRUE)
  }
  x
}

as_graph.list = function(x, deep_copy = FALSE) {
  gunion(x)  # gunion itself will convert individual members of x
}

as_graph.Graph = function(x, deep_copy = FALSE) {
  if (deep_copy) {
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
#' If `deep_copy` is `TRUE`, a deep copy is made
#' if the incoming object is a `PipeOp` to ensure the resulting
#' object is a different reference from the incoming object.
#'
#' @param x (`any`) \cr
#'   Object to convert.
#' @param deep_copy (`logical(1)`) \cr
#'   Whether to return a deep copy if `x` is a PipeOp.
#' @return [`PipeOp`] `x` or a deep clone of it.
#' @family Graph operators
#' @export
as_pipeop = function(x, deep_copy = FALSE) {
  UseMethod("as_pipeop")
}

as_pipeop.default = function(x, deep_copy = FALSE) {
  stopf("%s can not be converted to PipeOp", deparse(substitute(x))[1])
}

as_pipeop.PipeOp = function(x, deep_copy = FALSE) {
  if (deep_copy) {
    x = x$clone(deep = TRUE)
  }
  x
}

as_pipeop.character = function(x, deep_copy = FALSE) {
  assert_string(x)
  if (x %nin% c(mlr_pipeops$keys(), mlr_learners$keys())) {
    stopf("'%s' is neither in mlr_pipeops nor in mlr_learners.%s%s",
      x, did_you_mean(x, mlr_pipeops$keys()), did_you_mean(x, mlr_learners$keys()))
  }
  if (x %in% mlr_pipeops$keys()) {
    x = mlr_pipeops$get(x)
  } else {
    as_pipeop(mlr_learners$get(x))
  }
}

as_pipeop.Learner = function(x, deep_copy = FALSE) {
  PipeOpLearner$new(x)
}
