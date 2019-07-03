#' @title PipeOpSubsample
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
  public = list(
    initialize = function(id = "subsample", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("frac", default = 1, lower = 0, upper = Inf),
        ParamLgl$new("stratify", default = FALSE),
        ParamLgl$new("replace", default = FALSE)
      ))
      ps$values = list(frac = 1, stratify = FALSE, replace = FALSE)
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE)
    },

    train_task = function(task) {
      sample_safe = function(x, size, replace) {
        if (length(x) == 1) {
          if (!replace && size > 1) {
            stop("cannot take a sample larger than the population when 'replace = FALSE'")
          }
          rep_len(x, size)
        } else {
          sample(x, size, replace)
        }
      }
      if (!self$param_set$values$stratify) {
        keep = sample_safe(task$row_roles$use,
          ceiling(self$param_set$values$frac * task$nrow),
          replace = self$param_set$values$replace)
      } else {
        if (!inherits(task, "TaskClassif")) {
          stopf("Stratification not supported for %s", class(task))
        }
        splt = split(task$row_roles$use, task$data(cols = task$target_names))
        keep = unlist(map(splt, function(x) {
          sample_safe(x,
            ceiling(self$param_set$values$frac * length(x)),
            replace = self$param_set$values$replace)
        }))
      }
      self$state = list()
      task_filter_ex(task, keep)
    },

    predict_task = identity
  )
)

register_dictionary("pipeop", "subsample", PipeOpSubsample)
