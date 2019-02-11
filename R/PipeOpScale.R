#' @title PipeOpScale
#'
#' @name mlr_pipeop_scale
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Scales the data to mean = 0 (if `$center` parameter is `TRUE`) and standard deviation 1
#' (if `$scale` parameter is `TRUE`).
#'
#' See [base::scale] for details and parameters.
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpScale = R6Class("PipeOpScale",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "scale", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale", default = TRUE)
      ))
      super$initialize(id = id, param_set = ps, param_vals = param_vals)
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
    },

    train_dt = function(dt, levels) {
      sc = invoke(scale, as.matrix(dt), .args = self$param_set$values)
      self$state = list(
        center = attr(sc, "scaled:center") %??% 0,
        scale = attr(sc, "scaled:scale") %??% 1
      )
      sc
    },

    predict_dt = function(dt, levels) {
      t((t(dt) - self$state$center) / self$state$scale)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("scale", PipeOpScale)
