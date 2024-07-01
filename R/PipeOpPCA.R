#' @title Principle Component Analysis
#'
#' @usage NULL
#' @name mlr_pipeops_pca
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Extracts principle components from data. Only affects numerical features.
#' See [stats::prcomp()] for details.
#'
#' @section Construction:
#' ```
#' PipeOpPCA$new(id = "pca", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"pca"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features replaced by their principal components.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as the elements of the class [stats::prcomp],
#' with the exception of the `$x` slot. These are in particular:
#' * `sdev` :: `numeric`\cr
#'   The standard deviations of the principal components.
#' * `rotation` :: `matrix`\cr
#'   The matrix of variable loadings.
#' * `center` :: `numeric` | `logical(1)`\cr
#'   The centering used, or `FALSE`.
#' * `scale` :: `numeric` | `logical(1)`\cr
#'   The scaling used, or `FALSE`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `center` :: `logical(1)`\cr
#'   Indicating whether the features should be centered. Default is `TRUE`. See [`prcomp()`][stats::prcomp].
#' * `scale.` :: `logical(1)`\cr
#'   Whether to scale features to unit variance before analysis. Default is `FALSE`, but scaling is advisable. See [`prcomp()`][stats::prcomp].
#' * `rank.` :: `integer(1)`\cr
#'   Maximal number of principal components to be used. Default is `NULL`: use all components. See [`prcomp()`][stats::prcomp].
#'
#' @section Internals:
#' Uses the [`prcomp()`][stats::prcomp] function.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("pca")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpPCA = R6Class("PipeOpPCA",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "pca", param_vals = list()) {
      ps = ps(
        center = p_lgl(default = TRUE, tags = c("train", "pca")),
        scale. = p_lgl(default = FALSE, tags = c("train", "pca")),
        rank. = p_int(default = NULL, lower = 1, upper = Inf, special_vals = list(NULL), tags = c("train", "pca"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .train_dt = function(dt, levels, target) {
      pcr = invoke(stats::prcomp, as.matrix(dt), .args = self$param_set$get_values(tags = "pca"))
      self$state = pcr
      self$state$x = NULL
      pcr$x
    },

    .predict_dt = function(dt, levels) {
      stats::predict(self$state, as.matrix(dt))
    }
  )
)

mlr_pipeops$add("pca", PipeOpPCA)
