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
    initialize = function(innum, id = "featureunion") {
      assert_int(innum, lower = 1)
      super$initialize(id,
        input = data.table(name = rep_suffix("input", innum), train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      self$state = list()
      task = cbind_tasks(inputs)
      list(task)
    },

    predict = function(inputs) {
      task = cbind_tasks(inputs)
      list(task)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpFeatureUnion", PipeOpFeatureUnion)


#FIXME: this really should be suported by mlr3 and the code looks horrible
cbind_tasks = function(inputs) {
  task = inputs[[1L]]$clone(deep = TRUE)
  ids = task$row_ids
  inputs = discard(inputs, is.null)

  targets = unique(unlist(map(inputs, function(x) x$target_names), use.names = FALSE))
  # FIXME: we could also say we drop all targets except the first, maybe with a flag
  if (!setequal(targets, task$target_names))
    stopf("All tasks must have the same target columns")

  Reduce(function(x, y) {
    data = y$data(ids, y$feature_names)
    x$cbind(data)
  }, tail(inputs, -1L), init = task)
}
