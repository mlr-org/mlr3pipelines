
#' @title PipeOpRemoveConstants
#'
#' @usage NULL
#' @name mlr_pipeops_remove_constants
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Remove constant features from a [mlr3::Task].
#' For each feature, calculates the ratio of features which differ from their mode value.
#' Numeric features are optionally rounded first.
#' Missing values can be ignored or treated as a regular value.
#'
#' @section Construction:
#' ```
#' PipeOpRemoveConstants$new(id = "remove_constants")
#' ```
#'
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, defaulting to `"remove_constants"`.
#'
#' @section State:
#' `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `removed`: names of removed features
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpTaskPreproc`], as well as:
#' * `ratio`: Ratio of values which must be different from the mode value in order to keep a feature in the task.
#'   Default is 0, which means only constant features with exactly one observed level are removed.
#' * `digits`: Number of digits used to round numeric values before comparing them.
#'   Default is 8, which approximately corresponds to a tolerance of `sqrt(.Machine$double.eps)`.
#' * `na_ignore`: If `TRUE`, the ratio is calculated after removing all missing values first.
#'   Default is `FALSE`.
#'
#' @section Fields:
#' Fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library(mlr3)
#' data = data.table(y = runif(10), a = 1:10, b = rep(1, 10), c = rep(1:2, each = 5))
#'
#' task = TaskRegr$new("example", data, target = "y")
#'
#' po = po("remove_constants")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$feature_names
PipeOpRemoveConstants = R6Class("PipeOpRemoveConstants",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "remove_constants") {
      ps = ParamSet$new(list(
          ParamDbl$new("ratio", lower = 0, upper = 1, default = 0, tags = c("train", "predict", "required")),
          ParamInt$new("digits", lower = 0, tags = "train"),
          ParamLgl$new("na_ignore", default = FALSE, tags = c("train", "required"))
      ))
      ps$values = list(ratio = 0, na_ignore = TRUE)
      super$initialize(id, ps)
    },

    get_state = function(task) {
      pv = self$param_set$values
      ratios = map_dbl(task$data(), calculate_constness, digits = pv$digits, na_ignore = pv$na_ignore)
      list(ratios = ratios)
    },

    transform = function(task) {
      thresh = self$param_set$values$ratio
      drop = names(which(self$state$ratios <= thresh))
      task$select(setdiff(task$feature_names, drop))
    }
  )
)

mlr_pipeops$add("remove_constants", PipeOpRemoveConstants)

calculate_constness = function(x, digits = NULL, na_ignore = FALSE) {
  if (is.double(x) && !is.null(digits)) {
    x = round(x, digits)
  }

  if (na_ignore) {
    x = x[!is.na(x)]
    if (length(x) == 0L) {
      return(0)
    }
    return(mean(x != compute_mode(x, ties_method = "first")))
  }

  is_equal = function(x, y) {
    tmp = (x == y | (is.na(x) & is.na(y)))
    replace(tmp, is.na(tmp), FALSE)
  }

  mean(!is_equal(x, compute_mode(x, ties_method = "first", na_rm = FALSE)))
}
