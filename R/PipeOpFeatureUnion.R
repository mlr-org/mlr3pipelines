#' @title PipeOpFeatureUnion
#'
#' @usage NULL
#' @name mlr_pipeops_featureunion
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Aggregates features from all input tasks by [cbind()]ing them together into a single [`Task`][mlr3::Task].
#'
#' [`DataBackend`][mlr3::DataBackend] primary keys and [`Task`][mlr3::Task] targets have to be equal across all [`Task`][mlr3::Task]s.
#' Only the target column(s) of the first [`Task`][mlr3::Task] are kept.
#'
#' If `assert_targets_equal` is `TRUE` then target column names are compared and an error is thrown if they differ across inputs.
#'
#' @section Construction:
#' ```
#' PipeOpFeatureUnion$new(innum = 0, id = "featureunion", param_vals = list(), assert_targets_equal = TRUE)
#' ```
#'
#' * `innum` :: `numeric(1)` | `character`\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number of inputs.
#'   If `innum` is a `character` vector, the number of input channels
#'   is the length of `innum`, and the columns of the result are prefixed with the values.
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"featureunion"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#' * `assert_targets_equal` :: `logical(1)`\cr
#'   If `assert_targets_equal` is `TRUE` (Default),
#'   task target column names are checked for agreement. Disagreeing target column names are usually a
#'   bug, so this should often be left at the default.
#'
#' @section Input and Output Channels:
#' [`PipeOpFeatureUnion`] has multiple input channels depending on the `innum` construction argument, named `"input1"`, `"input2"`, ...
#' if `innum` is nonzero; if `innum` is 0, there is only one *vararg* input channel named `"..."`.
#' All input channels take a [`Task`][mlr3::Task] both during training and prediction.
#'
#' [`PipeOpFeatureUnion`] has one output channel named `"output"`, producing a [`Task`][mlr3::Task] both during training and prediction.
#'
#' The output is a [`Task`][mlr3::Task] constructed by `cbind()`ing all features from all input [`Task`][mlr3::Task]s, both during
#' training and prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpFeatureUnion`] has no Parameters.
#'
#' @section Internals:
#' [`PipeOpFeatureUnion`] uses the [`Task`][mlr3::Task] `$cbind()` method to bind the input values beyond the first
#' input to the first [`Task`][mlr3::Task]. This means if the [`Task`][mlr3::Task]s are database-backed, all of them
#' except the first will be fetched into R memory for this. This behaviour may change in the future.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' gr = gunion(list(
#'   po("nop"),
#'   po("pca")
#' )) %>>% po("featureunion")
#'
#' gr$train(task)
#'
#' po = po("featureunion", innum = c("a", "b"))
#'
#' po$train(list(task, task))
PipeOpFeatureUnion = R6Class("PipeOpFeatureUnion",
  inherit = PipeOp,
  public = list(
    assert_targets_equal = NULL,
    inprefix = NULL,
    initialize = function(innum = 0, id = "featureunion", param_vals = list(), assert_targets_equal = TRUE) {
      assert(
        check_int(innum, lower = 0),
        check_character(innum, min.len = 1, any.missing = FALSE)
      )
      if (is.numeric(innum)) {
        self$inprefix = rep("", innum)
      } else {
        self$inprefix = innum
        innum = length(innum)
      }
      assert_flag(assert_targets_equal)
      self$assert_targets_equal = assert_targets_equal
      inname = if (innum) rep_suffix("input", innum) else "..."
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = inname, train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
      private$add_tags("ensemble", overwrite = TRUE)
    },

    train_internal = function(inputs) {
      self$state = list()
      list(cbind_tasks(inputs, self$assert_targets_equal, self$inprefix))
    },

    predict_internal = function(inputs) {
      list(cbind_tasks(inputs, self$assert_targets_equal, self$inprefix))
    }
  )
)

mlr_pipeops$add("featureunion", PipeOpFeatureUnion)

cbind_tasks = function(inputs, assert_targets_equal, inprefix) {
  task = inputs[[1L]]
  ids = task$row_ids

  if (length(inprefix)) {  # inprefix has length 0 if innum is 0
    names(inputs) = inprefix
    if (inprefix[1] != "") {
      task$rename(task$feature_names, sprintf("%s.%s", inprefix[1], task$feature_names))
    }
  } else {
    names(inputs) = NULL
  }
  inputs = discard(inputs, is.null)

  targets = unique(unlist(map(inputs, function(x) x$target_names), use.names = FALSE))
  if (assert_targets_equal && !setequal(targets, task$target_names)) {
    stopf("All tasks must have the same target columns")
  }

  # cbind() with only empty data.tables is problematic, so we have to do voodoo magic here:
  # cbind at least one data.table that is guaranteed not to be empty and subtract that column later.
  new_cols = do.call(cbind, c(list(data.table(x = vector(length = task$nrow))), lapply(tail(inputs, -1), function(y) y$data(ids, y$feature_names))))[, -1, with = FALSE]


  task$clone(deep = TRUE)$cbind(new_cols)
}
