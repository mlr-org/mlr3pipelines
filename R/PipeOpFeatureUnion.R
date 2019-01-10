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
    innum = NULL,
    initialize = function(innum, id = "featureunion") {
      assert_int(innum, lower = 2L)
      super$initialize(id,
        input = data.table(name = rep_suffix("task", innum), train = "Task", predict = "Task"),
        output = data.table(name = "task", train = "Task", predict = "Task")
      )
      self$innum = innum
    },

    train = function(inputs) {
      self$state = list()
      cbind_tasks(inputs)
    },

    predict = function(inputs) {
      # FIXME: check that all types are equal
      # in theory we could down-convert, but that ewould probably be a bug on the user's side.
      if (is.data.frame(inputs[[1]])) {
        list(do.call(cbind, inputs))
      } else {
        cbind_tasks(inputs)
      }
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpFeatureUnion", PipeOpFeatureUnion)


#FIXME: this really should be suported by mlr3 and the code looks horrible
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

  data = do.call(cbind, c(all_data, input1$row_ids))
  list(task_update_data(input1, data))
}
