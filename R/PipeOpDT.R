#' @title PipeOpDT
#'
#' @name PipeOpDT
#' @format [R6Class] PipeOpDT
#'
#' @description
#' This let's us work with a [data.table] instead of a Task and
#' delegate all handling of [Task]s and [DataBackend] to the class.
#' Allows us to specify functions  `train_dt()` and `predict_dt` instead of
#' `train()` and `predict`, that expect the [data.table] containing only
#' the features from a [Task], and automatically
#' reconstruct the appropriate Task from a returned [data.table].
#' We thus enforce: [data.table] -> [data.table].
#' For examples see [PipeOpPCA] or [PipeOpScale].
#'
#' The underlying operation must not change row order or number.
#'
#' @family PipeOp
NULL

#' @include PipeOp.R
#' @export
PipeOpDT = R6Class("PipeOpDT",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "PipeOpDT", param_set = ParamSet$new()) {
      super$initialize(id, param_set = param_set,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      assert_function(self$train_dt, args = "dt")

      # Get feature dt from task
      task = inputs[[1L]]
      d = task$data(cols = task$feature_names)

      # Call train_dt function on features
      dt = as.data.table(self$train_dt(d))

      list(task = task$clone(deep = TRUE)$replace_features(dt))
    },

    predict = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      assert_function(self$predict_dt, args = "newdt")

      task = inputs[[1L]]
      d = task$data(cols = task$feature_names)

      # Call predict_dt function on features
      dt = as.data.table(self$predict_dt(d))

      return(list(task = task$clone(deep = TRUE)$replace_features(dt)))
    }
  )
)
