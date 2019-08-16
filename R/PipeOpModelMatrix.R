#' @title PipeOpModelMatrix
#'
#' @name mlr_pipeop_modelmatrix
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`].
#'
#' @description
#' Transforms columns using a given `formula`.
#' It therefore uses the `stats::model.matrix()` function.
#'
#' @section Parameter Set:
#' * `formula`  :: `formula` \cr Formula to use. Higher order interactions
#' can be created using constructs like `~. ^ 2`.
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpModelMatrix = R6Class("PipeOpModelMatrix",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "modelmatrix", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("formula")
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "stats")
    },

    transform_dt = function(dt, levels) {
      as.data.frame(model.matrix(self$param_set$values$formula, data = dt))
    }
  )
)

mlr_pipeops$add("modelmatrix", PipeOpModelMatrix)
