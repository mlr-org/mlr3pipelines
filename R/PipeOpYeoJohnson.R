#' @title Yeo-Johnson Transformation of Numeric Features
#'
#' @usage NULL
#' @name mlr_pipeops_yeojohnson
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Conducts a Yeo-Johnson transformation on numeric features. It therefore estimates
#' the optimal value of lambda for the transformation.
#' See [`bestNormalize::yeojohnson()`] for details.
#'
#' @section Construction:
#' ```
#' PipeOpYeoJohnson$new(id = "yeojohnson", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"yeojohnson"`.
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
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examplesIf requireNamespace("bestNormalize")
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("yeojohnson")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpYeoJohnson = R6Class("PipeOpYeoJohnson",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "yeojohnson", param_vals = list()) {
      ps = ps(
        eps = p_dbl(default = 0.001, lower = 0, tags = c("train", "yj")),
        standardize = p_lgl(default = TRUE, tags = c("train", "yj")),
        lower = p_dbl(tags = c("train", "yj")),
        upper = p_dbl(tags = c("train", "yj"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "bestNormalize", feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .train_dt = function(dt, levels, target) {
      bc = lapply(dt, FUN = function(x) {
        invoke(bestNormalize::yeojohnson, x, .args = self$param_set$get_values(tags = "yj"))
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
      for (j in colnames(dt)) {
        set(dt, j = j,
          value = stats::predict(self$state$bc[[j]], newdata = dt[[j]]))
      }
      dt
    }
  )
)


mlr_pipeops$add("yeojohnson", PipeOpYeoJohnson)
