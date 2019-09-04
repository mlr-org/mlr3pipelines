#' @title PipeOpYeoJohnson
#'
#' @usage NULL
#' @name mlr_pipeops_yeojohnson
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Conducts a Yeo-Johnson transformation on numeric features. It therefore estimates
#' the optimal value of lambda for the transformation.
#' See [bestNormalize::yeojohnson()] for details.
#'
#' @section Construction:
#' ```
#' PipeOpYeoJohnson$new(id = "yeojohnson", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"yeojohnson"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features replaced by their binded versions.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`],
#' as well as a list of class `yeojohnson` for each column, which is transformed.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `eps` :: `numeric(1)` \cr
#'   Tolerance parameter to identify the lambda parameter as zero.
#'   For details see [`yeojohnson()`][bestNormalize::yeojohnson].
#' * `standardize` :: `logical` \cr
#'   Whether to center and scale the transformed values to attempt a standard
#'   normal distribution. For details see [`yeojohnson()`][bestNormalize::yeojohnson].
#' * `lower` :: `numeric(1)` \cr
#'   Lower value for estimation of lambda parameter.
#'   For details see [`yeojohnson()`][bestNormalize::yeojohnson].
#' * `upper` :: `numeric(1)` \cr
#'   Upper value for estimation of lambda parameter.
#'   For details see [`yeojohnson()`][bestNormalize::yeojohnson].
#'
#' @section Internals:
#' Uses the [`bestNormalize::yeojohnson`] function.
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library(mlr3)
#'
#' task = tsk("iris")
#' pop = po("yeojohnson")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpYeoJohnson = R6Class("PipeOpYeoJohnson",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "yeojohnson", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("eps", default = 0.001, lower = 0, tags = "train"),
        ParamLgl$new("standardize", default = TRUE, tags = "train"),
        ParamDbl$new("lower", tags = "train"),
        ParamDbl$new("upper", tags = "train")
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "bestNormalize")
    },

    train_dt = function(dt, levels) {
      bc = lapply(dt, FUN = function(x) {
        invoke(bestNormalize::yeojohnson, x, .args = self$param_set$values)
      })
      self$state = bc
      cols = names(bc)
      for (j in cols) set(dt, j = j, value = bc[[j]]$x.t)
      return(dt)
    },

    predict_dt = function(dt, levels) {
      cols = colnames(dt)
      for (j in cols) set(dt, j = j,
        value = predict(self$state[[j]], newdata = dt[[j]]))
      return(dt)
    }
  )
)


mlr_pipeops$add("yeojohnson", PipeOpYeoJohnson)
