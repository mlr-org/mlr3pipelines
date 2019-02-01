#' @title No-Op Sentinel Used for Alternative Branching
#'
#' @format [`R6`] object.
#'
#' @description
#' Special data type for no-ops. Distinct from `NULL` for easier debugging
#' and distinction from unintentional `NULL` returns.
#'
#' @family Path Branching
#' @export
NO_OP = R6Class("NO_OP",
  public = list(
    initialize = function() {},
    print = function() cat("mlr3pipelines NO_OP indicator\n")
  ),
)$new()

#' @title Test for NO_OP
#'
#' @description
#' Test whether a given object is a [`NO_OP`].
#'
#' @param x `any` \cr
#'   Object to test.
#' @return `logical(1)`: Whether `x` is a `NO_OP`.
#' @family Path Branching
#' @export
is_noop = function(x) test_r6(x, "NO_OP")

#' @title Remove NO_OPs from a List
#'
#' @description
#' Remove all [`NO_OP`] elements from a `list`.
#'
#' @param x `list` \cr
#'   List to filter.
#' @return `list`: The input list, with all `NO_OP` elements removed.
#' @family Path Branching
#' @export
filter_noop = function(x) Filter(Negate(is_noop), x)
