#' @title Transform Columns by Constructing a Model Matrix
#'
#' @usage NULL
#' @name mlr_pipeops_modelmatrix
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Transforms columns using a given `formula` using the [stats::model.matrix()] function.
#'
#' @section Construction:
#' ```
#' PipeOpModelMatrix$new(id = "modelmatrix", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"modelmatrix"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with transformed columns according to the used `formula`.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `formula`  :: `formula` \cr
#'   Formula to use. Higher order interactions can be created using constructs like `~. ^ 2`.
#'   By default, an `(Intercept)` column of all `1`s is created, which can be avoided by adding `0 +` to the term.
#'   See [`model.matrix()`][stats::model.matrix()].
#'
#' @section Internals:
#' Uses the [`model.matrix()`][stats::model.matrix()] function.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("modelmatrix", formula = ~ .  ^ 2)
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$param_set$values$formula = ~ 0 + . ^ 2
#'
#' pop$train(list(task))[[1]]$data()
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpModelMatrix = R6Class("PipeOpModelMatrix",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "modelmatrix", param_vals = list()) {
      ps = ParamSet$new(params = list(
        formula = p_uty(tags = c("train", "predict"), custom_check = check_formula)
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "stats")
    }
  ),
  private = list(
    .transform_dt = function(dt, levels) {
      as.data.frame(stats::model.matrix(self$param_set$values$formula, data = dt))
    }
  )
)

mlr_pipeops$add("modelmatrix", PipeOpModelMatrix)
