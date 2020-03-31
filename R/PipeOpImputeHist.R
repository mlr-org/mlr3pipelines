#' @title PipeOpImputeHist
#'
#' @usage NULL
#' @name mlr_pipeops_imputehist
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute numerical features by histogram.
#'
#' @section Construction:
#' ```
#' PipeOpImputeHist$new(id = "imputehist", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputehist"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImputeHist`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features missing values imputed by (column-wise) histogram.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` is a named `list` of `list`s containing elements `$counts` and `$breaks`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`].
#'
#' @section Internals:
#' Uses the [`graphics::hist()`] function. Features that are entirely `NA` are imputed as `0`.
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
#' po = po("imputehist")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' po$state$model
#' @family PipeOps
#' @family Imputation PipeOps
#' @include PipeOpImpute.R
#' @export
PipeOpImputeHist = R6Class("PipeOpImputeHist",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputehist", param_vals = list()) {
      super$initialize(id, param_vals = param_vals, packages = "graphics")
    }
  ),
  private = list(

    .select_cols = function(task) task$feature_types[get("type") %in% c("numeric", "integer"), get("id")],

    .train_imputer = function(feature, type, context) {
      graphics::hist(feature, plot = FALSE)[c("counts", "breaks")]
    },

    .impute = function(feature, type, model, context) {
      which.bins = sample.int(length(model$counts), sum(is.na(feature)), replace = TRUE, prob = model$counts)
      sampled = runif(length(which.bins), model$breaks[which.bins], model$breaks[which.bins + 1L])
      if (type == "integer") {
        sampled = as.integer(round(sampled))
      }
      feature[is.na(feature)] = sampled
      feature
    }
  )
)

mlr_pipeops$add("imputehist", PipeOpImputeHist)
