#' @title PipeOpFeatureUnion
#'
#' @name mlr_pipeop_featureunion
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#'   Aggregates features from all input tasks by cbinding them together
#'   into a [`data.table`].
#'   [`DataBackend`] primary keys and [`Task`] targets have to be equal across each
#'   `Task`. Only one target columns is kept.
#'
#' @section Methods:
#' * `PipeOpFeatureUnion$new(innum, id = "featureunion")` \cr
#'   (`numeric(1)`, `character(1)`) -> `self` \cr
#'   Constructor. `innum` determines the number of input channels.
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpFeatureUnion = R6Class("PipeOpFeatureUnion",
  inherit = PipeOp,
  public = list(
    initialize = function(innum, id = "featureunion", param_vals = list()) {
      assert_int(innum, lower = 1)
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = rep_suffix("input", innum), train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      self$state = list()
      list(cbind_tasks(inputs))
    },

    predict = function(inputs) {
      list(cbind_tasks(inputs))
    }
  )
)


cbind_tasks = function(inputs) {
  task = inputs[[1L]]
  ids = task$row_ids
  inputs = discard(inputs, is.null)

  targets = unique(unlist(map(inputs, function(x) x$target_names), use.names = FALSE))
  if (!setequal(targets, task$target_names))
    stopf("All tasks must have the same target columns")

  new_cols = Reduce(function(x, y) ref_cbind(x, y$data(ids, y$feature_names)), tail(inputs, -1L), init = data.table())
  task$clone(deep = TRUE)$cbind(new_cols)
}
