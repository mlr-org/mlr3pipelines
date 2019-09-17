#' @title PipeOpImputeSample
#'
#' @usage NULL
#' @name mlr_pipeops_imputesample
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute features by sampling from non-missing training data.
#'
#' @section Construction:
#' ```
#' PipeOpImputeSample$new(id = "imputesample", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputesample"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImputeSample`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features missing values imputed by values sampled (column-wise) from training data.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` is a named `list` of training data with missings removed.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`].
#'
#' @section Internals:
#' Uses the `sample()` function. Features that are entirely `NA` are imputed as the values given by `vector()` of their type.
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
#' po = po("imputesample")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpImputeSample = R6Class("PipeOpImputeSample",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputesample", param_vals = list()) {
      super$initialize(id, param_vals = param_vals)
    },

    train_imputer = function(feature, type, context) {
      feature[!is.na(feature)]
    },

    impute = function(feature, type, model, context) {
      outlen = sum(is.na(feature))
      filldata = if (!length(model)) {
        vector(type, outlen)
      } else if (length(model) == 1) {
        rep_len(model, outlen)
      } else {
        sample(model, outlen, replace = TRUE)
      }
      feature[is.na(feature)] = filldata
      feature
    }
  )
)

mlr_pipeops$add("imputesample", PipeOpImputeSample)
