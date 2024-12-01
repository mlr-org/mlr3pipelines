#' @title Dictionary of (sub-)graphs
#'
#' @usage NULL
#' @format [`R6Class`][R6::R6Class] object inheriting from [`mlr3misc::Dictionary`].
#'
#' @description
#' A simple [`Dictionary`][mlr3misc::Dictionary] storing objects of class [`Graph`].
#' The dictionary contains a collection of often-used graph structures, and it's aim
#' is solely to make often-used functions more accessible.
#' Each `Graph` has an associated help page, which can be accessed via `?mlr_graphs_<key>`, i.e.
#' `?mlr_graphs_bagging`.
#'
#' @section Methods:
#' Methods inherited from [`Dictionary`][mlr3misc::Dictionary], as well as:
#' * `add(key, value)`\cr
#'   (`character(1)`, `function`)\cr
#'   Adds constructor `value` to the dictionary with key `key`, potentially
#'   overwriting a previously stored item.
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [`Dictionary`][mlr3misc::Dictionary] -> [`data.table::data.table`]\cr
#'   Returns a `data.table` with column `key` (`character`).
#' @family mlr3pipelines backend related
#' @family Dictionaries
#' @export
#' @examplesIf requireNamespace("rpart")
#' library(mlr3)
#' lrn = lrn("regr.rpart")
#' task = mlr_tasks$get("boston_housing")
#'
#' # Robustify the learner for the task.
#' gr = pipeline_robustify(task, lrn) %>>% po("learner", lrn)
#' # or equivalently
#' gr = mlr_graphs$get("robustify", task = task, learner = lrn) %>>% po(lrn)
#' # or equivalently
#' gr = ppl("robustify", task, lrn) %>>% po("learner", lrn)
#'
#' # all Graphs currently in the dictionary:
#' as.data.table(mlr_graphs)
mlr_graphs = R6Class("DictionaryGraph", inherit = mlr3misc::Dictionary,
  cloneable = FALSE,
  public = list(
    add = function(key, value) {
      assert_function(value)
      ret = super$add(key, value)
      invisible(self)
    }
))$new()

#' @export
as.data.table.DictionaryGraph = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    list(key = key)
  }), "key")[]
}
