#' @title Normalize Data Row-wise
#'
#' @usage NULL
#' @name mlr_pipeops_spatialsign
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Normalizes the data row-wise. This is a natural generalization of the "sign" function to higher dimensions.
#'
#' @section Construction:
#' ```
#' PipeOpSpatialSign$new(id = "spatialsign", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"spatialsign"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features replaced by their normalized versions.
#'
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `length`  :: `numeric(1)` \cr
#'  Length to scale rows to. Default is 1.
#' * `norm` :: `numeric(1)` \cr
#'  Norm to use. Rows are scaled to `sum(x^norm)^(1/norm) == length` for finite `norm`, or to `max(abs(x)) == length`
#'  if `norm` is `Inf`. Default is 2.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#'
#' task$data()
#'
#' pop = po("spatialsign")
#'
#' pop$train(list(task))[[1]]$data()
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSpatialSign = R6Class("PipeOpSpatialSign",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "spatialsign", param_vals = list()) {
      ps = ps(
        length = p_dbl(tags = c("train", "predict"), lower = 0),
        norm = p_dbl(tags = c("train", "predict"), lower = 0)
      )
      ps$values = list(norm = 2, length = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(
    .transform_dt = function(dt, levels) {
      if (!nrow(dt)) {
        # if dt has no rows then we still have to convert columns to numeric.
        return(dt[, lapply(.SD, as.numeric)])
      }
      norm = self$param_set$values$norm
      t(apply(dt, 1, function(x) {
        if (is.finite(norm)) {
          len = sum(abs(x) ^ norm) ^ (1 / norm)
        } else {
          len = max(abs(x))
        }
        if (len != 0) {
          x = x / len * self$param_set$values$length
        }
        x
      }))
    }
  )
)

mlr_pipeops$add("spatialsign", PipeOpSpatialSign)
