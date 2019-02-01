#' @title PipeOpPCA
#'
#' @name mlr_pipeop_pca
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Extracts principle components from data.
#'
#' See [stats::prcomp] for details  and parameters.
#' @examples
#' # Instantiate PipeOpPCA
#' op1 = PipeOpPCA$new()
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpPCA = R6Class("PipeOpPCA",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "pca") {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale.", default = FALSE),
        ParamInt$new("rank.", default = NULL, lower = 1, upper = Inf, special_vals = list(NULL))
      ))
      super$initialize(id, param_set = ps)
    },

    select_cols = function(task) {
      task$feature_types[get("type") == "numeric", get("id")]
    },

    train_dt = function(dt) {
      pcr = invoke(stats::prcomp, as.matrix(dt), .args = self$param_vals)
      self$state = pcr
      self$state$x = NULL
      pcr$x
    },

    predict_dt = function(dt) {
      predict(self$state, as.matrix(dt))
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("pca", PipeOpPCA)
