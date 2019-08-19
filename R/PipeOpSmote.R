#' @title PipeOpSmote
#'
#' @usage NULL
#' @name mlr_pipeops_smote
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Adds features according to expressions given as formulas that may depend on values of other features.
#' This can add new features, or can change existing features.
#'
#' @section Construction:
#' ```
#' PipeOpSmote$new(id = "smote", param_vals = list())
#' ```
#" * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"smote"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with added and/or smoted features according to the `mutation` parameter.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `mutation` :: named `list` of `formula`\cr
#'   Expressions for new features to create (or present features to change), in the form of `formula`.
#'   Each element of the list is a `formula` with the name of the element naming the feature to create or
#'   change, and the formula expression determining the result. This expression may reference
#'   other features, as well as variables visible at the creation of the `formula` (see examples).
#'   Initialized to `list()`.
#' * `delete_originals` :: `logical(1)` \cr
#'   Whether to delete original features. Even when this is `FALSE`,
#'   present features may still be overwritten. Initialized to `FALSE`.
#'
#' @section Internals:
#' A `formula` created using the `~` operator always contains a reference to the `environment` in which
#' the `formula` is created. This makes it possible to use variables in the `~`-expressions that both
#' reference either column names or variable names.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' pom = mlr_pipeops$get("smote")
#' pom$param_set$values$mutation = list(
#'   Sepal.Area = ~ Sepal.Width * Sepal.Length,
#'   Petal.Area = ~ Petal.Width * Petal.Length
#' )
#'
#' pom$train(list("iris"))[[1]]$data()
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSmote = R6Class("PipeOpSmote",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "smote", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("form", tags = "required"),
        ParamInt$new("perc.over", default = 200),
        ParamInt$new("k", default = 5),
        ParamInt$new("perc.under", default = 200),
        ParamUty$new("learner", default = NULL)
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "DMwR")
    },

    train_task = function(task) {
      ps = self$param_set$values
      dt_old = task$data()
      ps$data = dt_old
      dt = as.data.table(invoke(DMwR::SMOTE, .args = ps))
      ###### TO DO: Add new data to task########
      return(task)
    }
  )
)

mlr_pipeops$add("smote", PipeOpSmote)
