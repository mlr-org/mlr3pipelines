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
#' * `applicator_vector` :: `function` \cr
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
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "apply", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("applicator_vector", custom_check = check_function_or_null),
        ParamUty$new("applicator_single", custom_check = check_function_or_null)
      ))
      super$initialize(id, ps, param_vals = param_vals)
    },

    # we can not inherit PipeOpTaskPreprocSimple here, because if `applicator_single` is given
    # and a prediction data table has 0 rows, then the resulting data.table does not know
    # what the column types should be. Here we enforce column type conformity in that case
    # by simply saving a copy of an empty dt.
    train_dt = function(dt, levels) {
      dt = self$transform_dt(dt, levels)
      self$state = list(emptydt = dt[integer(0)])
      dt
    },

    predict_dt = function(dt, levels) {
      dt = self$transform_dt(dt, levels)
      if (!nrow(dt)) {
        dt = self$state$emptydt
      }
      dt
    },

    transform_dt= function(task, task_levels) {
      applicator = self$param_set$values$applicator_vector
      applicator_single = self$param_set$values$applicator_single
      if (is.null(applicator) == is.null(applicator_single)) {
        stop("Exactly one of 'applicator' or 'applicator_single' must be given.")
      }
      applicator = applicator %??% function(x) sapply(x, applicator_single)
      task[, names(task) := lapply(task, applicator)]
    }
  )
)

mlr_pipeops$add("apply", PipeOpApply)

