#' @title PipeOpFeatureUnion
#'
#' @name PipeOpFeatureUnion
#' @format [R6Class] PipeOpFeatureUnion
#'
#' @description
#'   Aggregates features from all input tasks by cbinding them together
#'   into a [data.table].
#'   [DataBackend] primary keys and [Task] targets have to be equal across each
#'   task. Only one target is kept.
#' @section Usage:
#' Inherits from [PipeOp]
#' * `f = pipeOpFeatureUnion$new(id)` \cr
#'     `character(1)` -> [PipeOpFeatureUnion]
#' @family PipeOp
#' @family PipeOpAggregate
NULL

#' @include PipeOp.R
#' @export
PipeOpFeatureUnion = R6Class("PipeOpFeatureUnion",
  inherit = PipeOp,
  public = list(
    initialize = function(innum, id = "featureunion") {
      assert_int(innum, lower = 1)
      super$initialize(id,
        input = data.table(name = rep_suffix("task", innum), train = "Task", predict = "Task"),
        output = data.table(name = "task", train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      self$state = list()
      task = cbind_tasks(inputs)
      list(task = task)
    },

    predict = function(inputs) {
      task = cbind_tasks(inputs)
      list(task = task)
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
  if (!setequal(targets, task$target_names))
    stopf("All tasks must have the same target columns")

  Reduce(function(x, y) {
    data = y$data(ids, y$feature_names)
    x$cbind(data)
  }, tail(inputs, -1L), init = task)
}
