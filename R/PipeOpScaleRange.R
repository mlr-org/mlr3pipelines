#' @title PipeOpScaleRange
#'
#' @name mlr_pipeop_scalerange
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`].
#'
#' @description
#' Scales numeric data columns so their maximum absolute value
#' is \code{maxabs}, see [mlrCPO::cpoScaleRange] for details.
#'
#' @examples
#' # Instantiate PipeOpScaleRange
#' op1 = PipeOpScaleRange$new()
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpScaleRange = R6Class("PipeOpScaleRange",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "scalerange", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("lower", default = 1),
        ParamDbl$new("upper", default = 1)
      ))
      ps$values = list(lower = 0, upper = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals)
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
    },

    get_state_dt = function(dt, levels) {
      lapply(dt, function(x) {
        rng = range(x, na.rm = TRUE, finite = TRUE)
        # linear transformation to get minimum to 'lower' and maximum to 'upper:
        # x' = a + x * b
        # where b is (upper - lower) / (max(x) - min(x))
        # and   a is -min(x) * b + lower
        b = (self$param_set$values$upper - self$param_set$values$lower) / (rng[2] - rng[1])
        a = -rng[1] * b + self$param_set$values$lower
        c(a, b)
      })
    },

    transform_dt = function(dt, levels) {
      for (i in seq_along(dt)) {
        trafo = self$state[[i]]
        dt[[i]] = trafo[1] + dt[[i]] * trafo[2]
      }
      dt
    }
  )
)

mlr_pipeops$add("scalerange", PipeOpScaleRange)
