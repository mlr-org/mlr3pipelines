#' @title PipeOpDT
#' @format [R6Class] PipeOpDT
#'
#' @description
#' This let's us work with a [data.table] instead of a Task and
#' delegate all handling of [Task]s and [DataBackend] to the class.
#' Allows us to specify functions  `train_dt()` and `predict_dt` instead of
#' `train()` and `predict`, that expect the [data.table] containing only
#' the features from a [Task], and automatically
#' reconstruct the appropriate Task from a returned [data.table].
#' We thus enforce: [dt] -> [dt].
#' For examples see [PipeOpPCA] or [PipeOpScale].
#'
#' The underlying operation must not change row order or number.
#'
#' @name PipeOpDT
#' @family PipeOp
#' @export
PipeOpDT = R6Class("PipeOpDT",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "PipeOpDT", ps = ParamSet$new()) {
      super$initialize(id, ps)
      private$.intype = list("any")
      private$.outtype = list("any")
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      assert_function(self$train_dt, args = "dt")

      # Get feature dt from task
      task = inputs[[1L]]
      fn = task$feature_names
      d = task$data()

      # Call train_dt function on features
      dt = as.data.table(self$train_dt(d[, ..fn]))
      assert_true(nrow(dt) == nrow(d))

      list(task_update_data(task, dt))
    },

    predict = function() {
      assert_function(self$predict_dt, args = "newdt")
      assert(
          check_list(self$inputs, len = 1L, type = "Task"),
          check_list(self$inputs, len = 1L, type = "data.frame")
      )
      if (is.data.frame(self$inputs[[1]])) {
        indata = as.data.table(self$inputs[[1]])
      } else {
        task = self$inputs[[1L]]
        fn = task$feature_names
        indata = task$data()[, ..fn]
      }

      # Call train_dt function on features
      dt = as.data.table(self$predict_dt(indata))
      assert_true(nrow(dt) == nrow(indata))
      if (is.data.frame(self$inputs[[1]])) {
        if (!is.data.table(self$inputs[[1]])) {
          dt = as.data.frame(dt)
        }
        return(list(dt))
      }
      # Drop old features, add new features
      list(task_update_data(task, dt))
    }
  )
)
