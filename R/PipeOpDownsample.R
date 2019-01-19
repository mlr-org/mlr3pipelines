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
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "downsample") {
      ps = ParamSet$new(params = list(
        ParamDbl$new("frac", default = 1, lower = 0, upper = 1),
        ParamLgl$new("stratify", default = FALSE)
      ))
      super$initialize(id, param_set = ps, can_subset = FALSE)
      self$param_vals = list(frac = 1, stratify = FALSE)
    },

    train_task = function(task) {
      if (!self$param_vals$stratify) {
        keep = sample(task$row_roles$use, ceiling(self$param_vals$frac * task$nrow))
      } else {
        if (!inherits(task, "TaskClassif"))
          stopf("Stratification not supported for %s", class(task))
        splt = split(task$row_roles$use, task$data(cols = task$target_names))
        keep = unlist(map(splt, function(x) sample(x, ceiling(self$param_vals$frac * length(x)))))
      }
      self$state = list()
      task$filter(keep)
    },

    predict_task = identity
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpDownsample", PipeOpDownsample)
