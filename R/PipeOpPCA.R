#' @title PipeOpPCA
#' @format [R6Class] PipeOpPCA
#' @description
#'   Extracts principle components from data.
#'   See [stats::prcomp] for details  and parameters.
#' @section Usage:
#' Inherits from [PipeOpDT]
#' * `f = pipeOpPCA$new(id)` \cr
#'     `character(1)` -> [PipeOpPCA]
#' @name PipeOpPCA
#' @family PipeOp
#' @export
#' @examples
#' # Instantiate PipeOpPCA
#' op1 = PipeOpPCA$new()
PipeOpPCA = R6Class("PipeOpPCA",

  inherit = PipeOpDT,

  public = list(
    initialize = function(id = "pca") {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale.", default = FALSE),
        ParamInt$new("rank.", default = NULL, lower = 1, upper = Inf, special_vals = list(NULL))
      ))
      super$initialize(id, ps)
    },

    train_dt = function(dt) {
      pcr = prcomp(as.matrix(dt),
        center = self$param_vals$center,
        scale. = self$param_vals$scale.,
        rank.  = self$param_vals$rank.)
      self$state = pcr
      self$state$x = NULL
      pcr$x
    },

    predict_dt = function(newdt) {
      predict(self$state, as.matrix(newdt))
    }
  )
)
