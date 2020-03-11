#' @title PipeOpImputeMode
#'
#' @usage NULL
#' @name mlr_pipeops_imputemode
#' @format [`R6Class`] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute numerical features by their mode.
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
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features missing values imputed by (column-wise) mode.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$model` is a named `list` of `numeric(1)` indicating the mode of the respective feature.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`], as well as the following
#' parameters based on [`compute_mode()`][mlr3misc::compute_mode]:
#' * `ties_method` :: `character(1)`\cr
#'   Ties handling type. One of \dQuote{random} (default), \dQuote{first} or \dQuote{last}.
#'
#' @section Internals:
#' Uses the [`mlr3misc::compute_mode()`] function. Features that are entirely `NA` are imputed as `0`.
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
      ps = ParamSet$new(params = list(
        ParamFct$new("ties_method", levels = c("first", "last", "random"), default = "random", tags = c("train", "predict"))
      ))
      ps$values = list(ties_method = "random")
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "mlr3misc")
    },

    select_cols = function(task) task$feature_types[get("type") %in% c("numeric", "integer"), get("id")],

    train_imputer = function(feature, type, context) {
      mod = mlr3misc::compute_mode(feature, ties_method = self$param_set$values$ties_method)
      if (is.na(mod)) {
        mod = 0
      }
      if (type == "integer") {
        mod = as.integer(round(mod))
      }
      mod
    },

    impute = function(feature, type, model, context) {
      feature[is.na(feature)] = model
      feature
    }
  )
)

mlr_pipeops$add("imputemode", PipeOpImputeMode)
