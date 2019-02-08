#' @title PipeOpDownsample
#'
#' @name mlr_pipeop_downsample
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#'   Subsamples a [`Task`] to use only a fraction of the rows.
#'
#' @section Parameter Set:
#' * `frac` :: `numeric(1)` \cr
#'   Fraction of rows in the task to keep. Default 1.
#' * `stratify` :: `logical(1)` \cr
#'   Should the subsamples be stratified by target? Default `FALSE`.
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpDownsample = R6Class("PipeOpDownsample",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "downsample", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("frac", default = 1, lower = 0, upper = 1),
        ParamLgl$new("stratify", default = FALSE)
      ))
      ps$values = list(frac = 1, stratify = FALSE)
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE)
    },

    train_task = function(task) {
      if (!self$param_set$values$stratify) {
        keep = sample(task$row_roles$use, ceiling(self$param_set$values$frac * task$nrow))
      } else {
        if (!inherits(task, "TaskClassif"))
          stopf("Stratification not supported for %s", class(task))
        splt = split(task$row_roles$use, task$data(cols = task$target_names))
        keep = unlist(map(splt, function(x) sample(x, ceiling(self$param_set$values$frac * length(x)))))
      }
      self$state = list()
      task$filter(keep)
    },

    predict_task = identity
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("downsample", PipeOpDownsample)
