#' @title PipeOpFeatureUnion
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
#' @name PipeOpFeatureUnion
#' @family PipeOp
#' @export
PipeOpFeatureUnion = R6Class("PipeOpFeatureUnion",

  inherit = PipeOp,

  public = list(
    initialize = function(innum, id = "featureunion") {
      self$train_intypes = rep("Task", innum)
      self$train_outtypes = "Task"
      self$predict_intypes = rep("Task", innum)
      self$predict_outtypes = "Task"
      super$initialize(id)
    },

    train = function(inputs) {
      self$state = list()
      cbind_tasks(inputs)
    },

    predict = function(inputs) {
      # FIXME: check that all types are equal
      # in theory we could down-convert, but that ewould probably be a bug on the user's side.
      if (is.data.frame(input[[1]])) {
        list(do.call(cbind, inputs))
      } else {
        cbind_tasks(inputs)
      }
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpFeatureUnion", PipeOpFeatureUnion)


cbind_tasks = function(inputs) {
  # FIXME: I think we should cbind the DataBackends instead.
  inputs = Filter(Negate(is.null), inputs)
  ## check if all target_names are equal
  is_target_equal = length(unique(vapply(
    inputs,
    function(x) digest::digest(x$target_names),
    FUN.VALUE = ""
  ))) == 1
  assert(is_target_equal)
  # FIXME: assert tasks agree on other characteristics:
  #  - row IDs
  #  - task types
  #  - ???

  all_data = lapply(inputs, function(x) {
    x$data()[, x$feature_names, with = FALSE]
  })

  input1 = inputs[[1]]
  targets = input1$data()[, input1$target_names, with = FALSE]

  data = do.call(cbind, c(all_data, input1$row_ids))
  list(task_update_data(input1, data))
}
