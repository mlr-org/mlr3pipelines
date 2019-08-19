#' @title PipeOpScaleMaxAbs
#'
#' @name mlr_pipeop_scalemaxabs
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`].
#'
#' @description
#' Scales numeric data columns so their maximum absolute value
#' is \code{maxabs}, see [mlrCPO::cpoScaleMaxAbs] for details.
#'
#' @examples
#' # Instantiate PipeOpScaleMaxAbs
#' op1 = PipeOpScaleMaxAbs$new()
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpScaleMaxAbs = R6Class("PipeOpScaleMaxAbs",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "scalemaxabs", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("maxabs", lower = 0, default = 1)
      ))
      ps$values = list(maxabs = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals)
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
    },

    get_state_dt = function(dt, levels) {
      lapply(dt, function(x){
        s = max(abs(range(x, na.rm = TRUE, finite = TRUE)))
        if (s == 0) {
          s = 1
        }
        s
      })
    },

    transform_dt = function(dt, levels) {
      for (i in seq_along(dt)) {
        dt[[i]] = dt[[i]] / self$state[[i]] * self$param_set$values$maxabs
      }
      dt
    }
  )
)

mlr_pipeops$add("scalemaxabs", PipeOpScaleMaxAbs)


