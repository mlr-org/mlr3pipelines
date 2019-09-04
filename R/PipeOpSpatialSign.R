#' @title PipeOpSpatialSign
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
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library(mlr3)
#'
#' task = tsk("iris")
#'
#' task$data()
#'
#' pop = po("spatialsign")
#'
#' pop$train(list(task))[[1]]$data()
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSpatialSign = R6Class("PipeOpSpatialSign",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "spatialsign", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("length", tags = c("train", "predict"), default = 1)
      ))
      ps$values = list(length = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals)
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
    },

    transform_dt = function(dt, levels) {
      if (nrow(dt) == 0) {
        return(dt)
      }
      res = t(apply(dt, 1, function(x) {
        len = sqrt(sum(x ^ 2))
        if (!identical(len, 0)) {
          x = x / len * self$param_set$values$length
        }
        x
      }))
      res = as.data.table(res)
      res
    }
  )
)

mlr_pipeops$add("spatialsign", PipeOpSpatialSign)
