#' @title PipeOpDownsample
#' @format [R6Class] PipeOpDownsample
#'
#' @description
#'   Subsamples a [Task] to include only a fraction of the rows.
#' @section Usage:
#' Inherits from [PipeOp]
#' * `f = pipeOpDownsample$new(id)` \cr
#' @section Details:
#' * `perc`: `numeric(1)` Percentage of rows in the task to keep.
#' * `stratify`: `logical(1)` Should the subsamples be stratified.
#' @name PipeOpDownsample
#' @family PipeOp
#' @export
PipeOpDownsample = R6Class("PipeOpDownsample",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "downsample") {
      ps = ParamSet$new(params = list(
        ParamNum$new("perc", default = 0.7, lower = 0, upper = 1),
        ParamLgl$new("stratify", default = FALSE)
      ))
      super$initialize(id, ps)
      private$.intype = list("any")
      private$.outtype = list("any")

    },

    train = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      # FIXME: this is really bad code how i change the data of the task
      # ich muss hier das backend austauschen
      task = self$inputs[[1L]]
      fn = task$feature_names
      # FIXME: Discuss whether we want to use the current mlr implementation
      list(TaskClassif$new(id = task$id, data = d, target = task$target_names))
    },

    predict = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      # FIXME: Make sure dimensions fit (if input did not have full rank)
      list(as.data.frame(as.matrix(input) %*% self$params))
    }
  )
)
