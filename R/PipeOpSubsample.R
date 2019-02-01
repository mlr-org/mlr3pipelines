#' @title PipeOpSample
#'
#' @name mlr_pipeop_subsample
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Subsamples a [`Task`] to use a fraction of the rows. `frac`
#' can be > 1 if `replace` is `TRUE`.
#'
#' @section Parameter Set:
#' * `frac` :: `numeric(1)` \cr
#'   Fraction of rows in the task to keep. Default 1.
#' * `stratify` :: `logical(1)` \cr
#'   Should the subsamples be stratified by target? Default `FALSE`.
#' * `replace` :: `logical(1)` \cr
#'   Sample with replacement? Default is `FALSE`.
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSubsample = R6Class("PipeOpSubsample",
  inherit = PipeOpTaskPreproc,

#FIXME: remove stratify for now? does not work for regression.
# or robustify it

  public = list(
    initialize = function(id = "subsample") {
      ps = ParamSet$new(params = list(
        ParamDbl$new("frac", default = 1, lower = 0, upper = Inf),
        ParamLgl$new("stratify", default = FALSE),
        ParamLgl$new("replace", default = FALSE)
      ))
      super$initialize(id, param_set = ps, can_subset_cols = FALSE)
      self$param_vals = list(frac = 1, stratify = FALSE, replace = FALSE)
    },

    train_task = function(task) {
      if (!self$param_vals$stratify) {
        keep = sample(task$row_roles$use, ceiling(self$param_vals$frac * task$nrow), replace = self$param_vals$replace)
      } else {
        if (!inherits(task, "TaskClassif"))
          stopf("Stratification not supported for %s", class(task))
        splt = split(task$row_roles$use, task$data(cols = task$target_names))
        keep = unlist(map(splt, function(x) sample(x, ceiling(self$param_vals$frac * length(x)), replace = self$param_vals$replace)))
      }
      self$state = list()
      task$filter(keep)
    },

    predict_task = identity
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("subsample", PipeOpSubsample)
