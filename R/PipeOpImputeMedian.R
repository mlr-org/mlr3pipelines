#' @title PipeOpImputeMedian
#'
#' @usage NULL
#' @name mlr_pipeops_imputemedian
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute numerical features by their median.
#'
#' @section Construction:
#' ```
#' PipeOpImputeMedian$new(id = "imputemedian", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputemedian"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImpute`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features missing values imputed by (column-wise) median.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` is a named `list` of `numeric(1)` indicating the median of the respective feature.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`].
#'
#' @section Internals:
#' Uses the [`stats::median()`] function. Features that are entirely `NA` are imputed as `0`.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("pima")
#' task$missings()
#'
#' po = po("imputemedian")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' po$state$model
#' @family PipeOps
#' @family Imputation PipeOps
#' @include PipeOpImpute.R
#' @export
PipeOpImputeMedian = R6Class("PipeOpImputeMedian",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputemedian", param_vals = list()) {
      super$initialize(id, param_vals = param_vals, packages = "stats")
    }
  ),
  private = list(

    .select_cols = function(task) function(task) selector_type(c("numeric", "integer"))(task),

    .train_imputer = function(feature, type, context) {
      med = stats::median(feature, na.rm = TRUE)
      if (is.na(med)) {
        med = 0
      }
      if (type == "integer") {
        med = as.integer(round(med))
      }
      med
    },

    .impute = function(feature, type, model, context) {
      feature[is.na(feature)] = model
      feature
    }
  )
)

mlr_pipeops$add("imputemedian", PipeOpImputeMedian)
