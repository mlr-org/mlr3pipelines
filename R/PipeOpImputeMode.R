#' @title PipeOpImputeMode
#'
#' @usage NULL
#' @name mlr_pipeops_imputemode
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute features by their mode. Supports factors as well as logical and numerical features.
#' If multiple modes are present then imputed values are sampled randomly from them.
#'
#'
#'
#' @section Construction:
#' ```
#' PipeOpImputeMode$new(id = "imputemode", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputemode"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImpute`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected features missing values imputed by (column-wise) mode.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` is a named `list` of a vector of length one of the type of the feature, indicating the mode of the respective feature.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`].
#'
#' @section Internals:
#' Features that are entirely `NA` are imputed as
#' the following: For `factor` or `ordered`, random levels are sampled uniformly at random.
#' For logicals, `TRUE` or `FALSE` are sampled uniformly at random.
#' Numerics and integers are imputed as `0`.
#'
#' Note that every random imputation is drawn independently, so different values may be imputed
#' if multiple values are missing.
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
#' po = po("imputemode")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' po$state$model
#' @family PipeOps
#' @family Imputation PipeOps
#' @include PipeOpImpute.R
#' @export
PipeOpImputeMode = R6Class("PipeOpImputeMode",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputemode", param_vals = list()) {
      super$initialize(id, param_vals = param_vals, packages = "mlr3misc")
    }
  ),
  private = list(

    .select_cols = function(task) task$feature_types[get("type") %in% c("factor", "integer", "logical", "numeric", "ordered"), get("id")],

    .train_imputer = function(feature, type, context) {
      feature = feature[!is.na(feature)]
      as.data.table(feature)[, .N, by = list(feature)][get("N") == max(get("N"))]$feature
    },

    .impute = function(feature, type, model, context) {
      if (length(model) == 1) {
        feature[is.na(feature)] = model
      } else {
        outlen = sum(is.na(feature))
        feature[is.na(feature)] = sample(model, outlen, replace = TRUE)
      }
      feature
    }
  )
)

mlr_pipeops$add("imputemode", PipeOpImputeMode)
