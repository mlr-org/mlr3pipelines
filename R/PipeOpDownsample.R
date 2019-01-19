#' @title PipeOpDownsample
#'
#' @name PipeOpDownsample
#' @format [R6Class] PipeOpDownsample
#'
#' @description
#'   Subsamples a [Task] to use only a fraction of the rows.
#' @section Usage:
#' Inherits from [PipeOp]
#' * `f = pipeOpDownsample$new(id)` \cr
#' @section Details:
#' * `frac`: `numeric(1)` Fracentage of rows in the task to keep. Default 1.
#' * `stratify`: `logical(1)` Should the subsamples be stratified by target? Default FALSE.
#' @family PipeOp
NULL

#FIXME: remove stratify for now? does not work for regression.
# or robustify it

#' @include PipeOp.R
#' @export
PipeOpDownsample = R6Class("PipeOpDownsample",
  inherit = PipeOp,

  public = list(
    initialize = function(id = "downsample") {
      ps = ParamSet$new(params = list(
        ParamDbl$new("frac", default = 1, lower = 0, upper = 1),
        ParamLgl$new("stratify", default = FALSE)
      ))
      super$initialize(id, param_set = ps,
        input = data.table(name = "task", train = "Task", predict = "Task"),
        output = data.table(name = "task", train = "Task", predict = "Task")
      )
      self$param_vals = list(frac = 1, stratify = FALSE)
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      tsk = inputs[[1]]$clone()
      if (!self$param_vals$stratify) {
        keep = sample(tsk$row_roles$use, ceiling(self$param_vals$frac * tsk$nrow))
      } else {
        if (!inherits(tsk, "TaskClassif"))
          stopf("Stratification not supported for %s", class(tsk))
        splt = split(tsk$row_roles$use, tsk$data(cols = tsk$target_names))
        keep = unlist(map(splt, function(x) sample(x, ceiling(self$param_vals$frac * length(x)))))
      }
      tsk$filter(keep)
      self$state = list()
      return(list(tsk))
    },

    predict = function(inputs) {
      return(inputs)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpDownsample", PipeOpDownsample)
