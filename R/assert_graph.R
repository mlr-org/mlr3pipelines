
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
assert_graph = function(x, coerce = FALSE, deep_copy = FALSE) {
  assert_flag(coerce)
  assert_flag(deep_copy)

  if (coerce && inherits(x, "PipeOp")) {
    x = Graph$new()$add_pipeop(x)
    deep_copy = FALSE  # add_pipeop already copies
  }
  assert_r6(x, "Graph")
  if (deep_copy) {
    x = x$clone(deep = TRUE)
  }
  invisible(x)
}
