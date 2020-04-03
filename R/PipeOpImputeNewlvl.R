#' @title PipeOpImputeNewlvl
#'
#' @usage NULL
#' @name mlr_pipeops_imputenewlvl
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute factorial features by adding a new level `".MISSING"`.
#'
#' @section Construction:
#' ```
#' PipeOpImputeNewlvl$new(id = "imputenewlvl", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"imputenewlvl"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImpute`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected factorial features missing values imputed by a new level.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` contains only `NULL` elements.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`].
#'
#' @section Internals:
#' Adds an explicit new `level()` to `factor` and `ordered` features, but not to `character` features.
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
#' po = po("imputenewlvl")
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' @family PipeOps
#' @family Imputation PipeOps
#' @include PipeOpImpute.R
#' @export
PipeOpImputeNewlvl = R6Class("PipeOpImputeNewlvl",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "imputenewlvl", param_vals = list()) {
      super$initialize(id, param_vals = param_vals)
    }
  ),
  private = list(

    # this is one of the few imputers that handles 'character' features!
    .select_cols = function(task) task$feature_types[get("type") %in% c("factor", "ordered", "character"), get("id")],

    .train_imputer = function(feature, type, context) {
      NULL
    },

    .impute = function(feature, type, model, context) {
      if (is.factor(feature)) {
        levels(feature) = c(levels(feature), ".MISSING")
      }
      feature[is.na(feature)] = ".MISSING"
      feature
    }
  )
)

mlr_pipeops$add("imputenewlvl", PipeOpImputeNewlvl)
