#' @title PipeOpFeatureUnion
#'
#' @name mlr_pipeop_featureunion
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#'   Aggregates features from all input tasks by cbinding them together
#'   into a [`data.table`].
#'   [`DataBackend`] primary keys and [`Task`] targets have to be equal across each
#'   `Task`. Only the target column(s) of the first task are kept.
#'   If `assert_targets_equal` is `TRUE`, then an error is thrown if target column name(s)
#'   disagree.
#'
#' @section Methods:
#' * `PipeOpFeatureUnion$new(innum, id = "featureunion", param_vals = list(), assert_targets_equal = TRUE)` \cr
#'   (`numeric(1)`, `character(1)`, named `list`, `logical(1)`) -> `self` \cr
#'   Constructor. `innum` determines the number of input channels. If `assert_targets_equal` is `TRUE` (Default),
#'   task target column names are checked for agreement. Disagreeing target column names are usually a
#'   bug, so this should often be left at the default.
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpFeatureUnion = R6Class("PipeOpFeatureUnion",
  inherit = PipeOp,
  public = list(
    assert_targets_equal = NULL,
    initialize = function(innum = 0, id = "featureunion", param_vals = list(), assert_targets_equal = TRUE) {
      assert_int(innum, lower = 0)
      assert_flag(assert_targets_equal)
      self$assert_targets_equal = assert_targets_equal
      inname = if (innum) rep_suffix("input", innum) else "..."
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = inname, train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      self$state = list()
      list(cbind_tasks(inputs, self$assert_targets_equal))
    },

    predict = function(inputs) {
      list(cbind_tasks(inputs, self$assert_targets_equal))
    }
  )
)

register_pipeop("featureunion", PipeOpFeatureUnion)

cbind_tasks = function(inputs, assert_targets_equal) {
  task = inputs[[1L]]
  ids = task$row_ids
  inputs = discard(inputs, is.null)

  targets = unique(unlist(map(inputs, function(x) x$target_names), use.names = FALSE))
  if (assert_targets_equal && !setequal(targets, task$target_names)) {
    stopf("All tasks must have the same target columns")
  }

  new_cols = Reduce(function(x, y) rcbind(x, y$data(ids, y$feature_names)), tail(inputs, -1L), init = data.table())
  task$clone(deep = TRUE)$cbind(new_cols)
}
