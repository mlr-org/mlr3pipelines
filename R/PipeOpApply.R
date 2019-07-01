#' @title PipeOpApply
#'
#' @name mlr_pipeop_apply
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`].
#'
#' @description
#' Applies a function to each element of a task. Only one of `applicator`
#' or `applicator_single` may be non-`NULL`.
#'
#' @section Parameter Set:
#' * `applicator` :: `function` \cr
#'   Function to apply to each column of the task. The return value must have the
#'   same length as the input, i.e. vectorize over the input. A typical example would be `as.numeric`.
#' * `applicator_single` :: `function` \cr
#'   Function to apply to each data element of the task; does not need to vectorize
#'   (although a vectorizing function gives better performance when given to `applicator`).
#'   Note that vectorizing functions should be used as `applicator` instead for greater
#'   performance.
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpApply = R6Class("PipeOpApply",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "apply", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("applicator", custom_check = check_function),
        ParamUty$new("applicator_single", custom_check = check_function)
      ))
      super$initialize(id, ps, param_vals = param_vals)
    },

    transform_dt= function(task, task_levels) {
      applicator = self$param_set$values$applicator
      applicator_single = self$param_set$values$applicator_single
      if (is.null(applicator) == is.null(applicator_single)) {
        stop("Exactly one of 'applicator' or 'applicator_single' must be given.")
      }
      applicator = applicator %??% function(x) sapply(x, applicator_single)
      task[, names(task) := lapply(task, applicator)]
    }
  )
)

register_pipeop("apply", PipeOpApply)
