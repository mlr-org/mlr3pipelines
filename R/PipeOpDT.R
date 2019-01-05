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

      # Drop old features, add new features
      d[, (fn) := NULL]
      d[, (colnames(dt)) := dt]
      d[, "..row_id" := seq_len(nrow(d))]

      db = DataBackendDataTable$new(d, primary_key = task$backend$primary_key)
      tn = task$target_names

      # Should be:
      list(TaskClassif$new(id = task$id, backend = db, target = tn))
    },

    predict = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      assert_function(self$predict_dt, args = "newdt")
      task = inputs[[1L]]
      fn = task$feature_names
      d = task$data()

      # Call train_dt function on features
      dt = as.data.table(self$predict_dt(d[, ..fn]))
      assert_true(nrow(dt) == nrow(d))

      # Drop old features, add new features
      d[, (fn) := NULL]
      d[, (colnames(dt)) := dt]
      d[, "..row_id" := seq_len(nrow(d))]

      db = DataBackendDataTable$new(d, primary_key = task$backend$primary_key)
      tn = task$target_names
      list(TaskClassif$new(id = task$id, backend = db, target = tn))
    }
  )
)
