#' @title PipeOpDownsample
#' @format [R6Class] PipeOpDownsample
#'
#' @description
#'   Subsamples a [Task] to use only a fraction of the rows.
#' @section Usage:
#' Inherits from [PipeOp]
#' * `f = pipeOpDownsample$new(id)` \cr
#' @section Details:
#' * `perc`: `numeric(1)` Percentage of rows in the task to keep.
#' * `stratify`: `logical(1)` Should the subsamples be stratified by target?
#' @name PipeOpDownsample
#' @family PipeOp
#' @export
PipeOpDownsample = R6Class("PipeOpDownsample",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "downsample") {
      ps = ParamSet$new(params = list(
        ParamDbl$new("perc", default = 0.7, lower = 0, upper = 1),
        ParamLgl$new("stratify", default = FALSE)
      ))
      super$initialize(id, ps)
      self$train_intypes = "Task"
      self$train_outtypes = "Task"
      self$predict_intypes = "Task"
      self$predict_outtypes = "Task"
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      tsk = inputs[[1]]$clone()
      if (!self$param_vals$stratify) {
        keep = sample(tsk$row_roles$use, ceiling(self$param_vals$perc * tsk$nrow))
      } else {
        splt = split(tsk$row_roles$use, tsk$data(cols = tsk$target_names))
        keep = unlist(map(splt, function(x) sample(x, ceiling(self$param_vals$perc * length(x)))))
      }
      tsk$set_row_role(setdiff(tsk$row_roles$use, keep), character(0))
      self$state = list()
      return(list(tsk))
    },
    predict = function(inputs) {return(inputs)}
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpDownsample", PipeOpDownsample)
