#' @title PipeOpScale
#'
#' @name PipeOpScale
#' @format [R6Class] PipeOpScale
#'
#' @description
#'   Scales the data to mean = 0 and standard deviation 1.
#'   See [base::scale] for details and parameters.
#' @section Usage:
#' Inherits from [PipeOpDT]
#' * `f = pipeOpDT$new(id)` \cr
#'     `character(1)` -> [PipeOpDT]
#' @family PipeOp
NULL

#' @include PipeOp.R
#' @export
PipeOpScale = R6Class("PipeOpScale",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "scale") {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale", default = TRUE)
      ))
      super$initialize(id, ps)
    },

    select_cols = function(task) {
      task$feature_types[get("type") == "numeric", get("id")]
    },

    train_dt = function(dt) {
      sc = scale(as.matrix(dt),
        center = self$param_vals$center,
        scale = self$param_vals$scale)

      self$state = list(
        center = attr(sc, "scaled:center") %??% 0,
        scale = attr(sc, "scaled:scale") %??% 1
      )
      dt
    },

    predict_dt = function(dt) {
      t((t(dt) - self$state$center) / self$state$scale)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpScale", PipeOpScale)
