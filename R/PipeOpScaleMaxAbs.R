#' @title Scale Numeric Features with Respect to their Maximum Absolute Value
#'
#' @usage NULL
#' @name mlr_pipeops_scalemaxabs
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Scales the numeric data columns so their maximum absolute value is `maxabs`,
#' if possible. `NA`, `Inf` are ignored, and features that are constant 0
#' are not scaled.
#'
#' @section Construction:
#' ```
#' PipeOpScaleMaxAbs$new(id = "scalemaxabs", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"scalemaxabs"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with scaled numeric features.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`],
#' as well as the maximum absolute values of each numeric feature.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `maxabs`  :: `numeric(1)` \cr
#'   The maximum absolute value for each column after transformation. Default is 1.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("scalemaxabs")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpScaleMaxAbs = R6Class("PipeOpScaleMaxAbs",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "scalemaxabs", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("maxabs", lower = 0, tags = c("required", "train", "predict"))
      ))
      ps$values = list(maxabs = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      lapply(dt, function(x) {
        s = max(abs(range(x, na.rm = TRUE, finite = TRUE)))
        if (s == 0) {
          s = 1
        }
        s
      })
    },

    .transform_dt = function(dt, levels) {
      for (i in seq_along(dt)) {
        dt[[i]] = dt[[i]] / self$state[[i]] * self$param_set$values$maxabs
      }
      dt
    }
  )
)

mlr_pipeops$add("scalemaxabs", PipeOpScaleMaxAbs)
