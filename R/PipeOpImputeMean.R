#' @title PipeOpImputeMean
#'
#' @usage NULL
#' @name mlr_pipeops_imputemean
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute numerical features by their mean.
#'
#' @section Construction:
#' ```
#' PipeOpImputeMean$new(id = "imputemean", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputemean"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImputeMean`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features missing values imputed by (column-wise) mean.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` is a named `list` of `numeric(1)` indicating the mean of the respective feature.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`].
#'
#' @section Internals:
#' Uses the `mean()` function. Features that are entirely `NA` are imputed as `0`.
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
#' po = po("imputemean")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' po$state$model
#' @family PipeOps
#' @family Imputation PipeOps
#' @include PipeOpImpute.R
#' @export
PipeOpImputeMean = R6Class("PipeOpImputeMean",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputemean", param_vals = list()) {
      super$initialize(id, param_vals = param_vals)
    },

    select_cols = function(task) task$feature_types[get("type") %in% c("numeric", "integer"), get("id")],

    train_imputer = function(feature, type, context) {
      men = mean(feature, na.rm = TRUE)
      if (is.nan(men)) {
        men = 0
      }
      if (type == "integer") {
        men = as.integer(round(men))
      }
      men
    },

    impute = function(feature, type, model, context) {
      feature[is.na(feature)] = model
      feature
    }
  )
)

mlr_pipeops$add("imputemean", PipeOpImputeMean)
