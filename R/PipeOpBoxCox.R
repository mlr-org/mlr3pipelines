#' @title Box-Cox Transformation of Numeric Features
#'
#' @usage NULL
#' @name mlr_pipeops_boxcox
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Conducts a Box-Cox transformation on numeric features. The lambda parameter
#' of the transformation is estimated during training and used for both training
#' and prediction transformation.
#' See [bestNormalize::boxcox()] for details.
#'
#' @section Construction:
#' ```
#' PipeOpBoxCox$new(id = "boxcox", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"boxcox"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features replaced by their transformed versions.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`],
#' as well as a list of class `boxcox` for each column, which is transformed.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `standardize` :: `logical(1)` \cr
#'   Whether to center and scale the transformed values to attempt a standard
#'   normal distribution. For details see [`boxcox()`][bestNormalize::boxcox].
#' * `eps` :: `numeric(1)` \cr
#'   Tolerance parameter to identify if lambda parameter is equal to zero.
#'   For details see [`boxcox()`][bestNormalize::boxcox].
#' * `lower` :: `numeric(1)` \cr
#'   Lower value for estimation of lambda parameter. For details see [`boxcox()`][bestNormalize::boxcox].
#' * `upper` :: `numeric(1)` \cr
#'   Upper value for estimation of lambda parameter. For details see [`boxcox()`][bestNormalize::boxcox].
#'
#' @section Internals:
#' Uses the [`bestNormalize::boxcox`] function.
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examplesIf requireNamespace("bestNormalize")
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("boxcox")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @template seealso_pipeopslist
#' @export
PipeOpBoxCox = R6Class("PipeOpBoxCox",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "boxcox", param_vals = list()) {
      ps = ps(
        standardize = p_lgl(default = TRUE, tags = c("train", "boxcox")),
        eps = p_dbl(default = 0.001, lower = 0, tags = c("train", "boxcox")),
        lower = p_dbl(tags = c("train", "boxcox")),
        upper = p_dbl(tags = c("train", "boxcox"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "bestNormalize", feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .train_dt = function(dt, levels, target) {
      bc = lapply(dt, FUN = function(x) {
        invoke(bestNormalize::boxcox, x, .args = self$param_set$get_values(tags = "boxcox"))
      })
      for (j in names(bc)) {
        set(dt, j = j, value = bc[[j]]$x.t)
        bc[[j]]$x.t = NULL
        bc[[j]]$x = NULL
      }
      self$state = list(bc = bc)
      dt
    },
    .predict_dt = function(dt, levels) {
      cols = colnames(dt)
      for (j in colnames(dt)) {
        set(dt, j = j,
          value = stats::predict(self$state$bc[[j]], newdata = dt[[j]]))
      }
      dt
    }
  )
)


mlr_pipeops$add("boxcox", PipeOpBoxCox)
