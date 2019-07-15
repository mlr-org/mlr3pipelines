#' @title Assertion for mlr3pipeline Graph
#'
#' @description
#' Function that checks that a given object is a `Graph` and
#' throws an error if not.
#'
#' If `coerce` is `TRUE`, the object is turned into a `Graph`
#' if possible before checking and the coerced object is
#' returned. If `deep_copy` is `TRUE`, a deep copy is made
#' before returning.
#'
#'
#' @param x (`any`) \cr
#'   Object to check.
#' @param coerce (`logical(1)`) \cr
#'   Whether to return a coerced object if possible instead
#'   of throwing an error.
#' @param deep_copy (`logical(1)`) \cr
#'   Whether to return a deep copy.
#' @return [`Graph`] `x` or a deep clone of it.
#' @family Graph operators
#' @export
assert_graph = function(x, coerce = FALSE, deep_copy = FALSE) {
  assert_flag(coerce)
  assert_flag(deep_copy)

  if (coerce && !inherits(x, "Graph")) {
    if (identical(class(x), "list")) {
      x = gunion(x)
    } else {
      x = Graph$new()$add_pipeop(x)  # add_pipeop checks automatically
    }
    # add_pipeop already copies, so no deep copy
    deep_copy = FALSE
  }
  assert_r6(x, "Graph")
  if (deep_copy) {
    x = x$clone(deep = TRUE)
  }
  invisible(x)
}

#' @title Assertion for mlr3pipeline PipeOp
#'
#' @description
#' Function that checks that a given object is a `PipeOp` and
#' throws an error if not.
#'
#' If `coerce` is `TRUE`, the object is turned into a `PipeOp`
#' if possible before checking and the coerced object is
#' returned. If `deep_copy` is `TRUE`, a deep copy is made
#' before returning.
#'
#'
#' @param x (`any`) \cr
#'   Object to check.
#' @param coerce (`logical(1)`) \cr
#'   Whether to return a coerced object if possible instead
#'   of throwing an error.
#' @param deep_copy (`logical(1)`) \cr
#'   Whether to return a deep copy.
#' @return [`Graph`] `x` or a deep clone of it.
#' @family Graph operators
#' @export
assert_pipeop = function(x, coerce = FALSE, deep_copy = FALSE) {
  if (!coerce) {
    assert_r6(x, "PipeOp")
  } else {
    if (is.character(x) && length(x) == 1) {
      if (x %nin% c(mlr_pipeops$keys(), mlr_learners$keys())) {
        stopf("'%s' is neither in mlr_pipeops nor in mlr_learners.%s%s",
          x, did_you_mean(x, mlr_pipeops$keys()), did_you_mean(x, mlr_learners$keys()))
      }
      if (x %in% mlr_pipeops$keys()) {
        x = mlr_pipeops$get(x)
      } else {
        x = mlr_learners$get(x)
      }
    }
    if (inherits(x, "R6ClassGenerator")) {
      x = x$new()
    }
    if (inherits(x, "Learner")) {
      x = PipeOpLearner$new(x)
    }
    invisible(cast_from_dict(x, "PipeOp", mlr_pipeops, deep_copy, FALSE)[[1L]])
  }
}
