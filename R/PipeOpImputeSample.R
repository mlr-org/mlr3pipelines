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
#' Input and output channels are inherited from [`PipeOpImpute`].
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
#' Uses the `sample()` function. Features that are entirely `NA` are imputed as
#' the following: For `factor` or `ordered`, random levels are sampled uniformly at random.
#' For logicals, `TRUE` or `FALSE` are sampled uniformly at random.
#' Numerics and integers are imputed as `0`.
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
#' @family Imputation PipeOps
#' @include PipeOpImpute.R
#' @export
PipeOpImputeSample = R6Class("PipeOpImputeSample",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputesample", param_vals = list()) {
      super$initialize(id, param_vals = param_vals)
    }
  ),
  private = list(

    .train_imputer = function(feature, type, context) {
      model = feature[!is.na(feature)]
      if (!length(model)) {
        model = switch(type,
          factor = levels(feature),
          integer = 0L, # see PipeOpImputeMean and PipeOpImputeMedian
          logical = c(TRUE, FALSE),
          numeric = 0, # see PipeOpImputeMean and PipeOpImputeMedian
          ordered = levels(feature)
        )
      }
      model
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

mlr_pipeops$add("imputesample", PipeOpImputeSample)
