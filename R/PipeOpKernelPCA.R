#' @title PipeOpKernelPCA
#'
#' @name mlr_pipeop_kernelpca
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Extracts kernel principle components from data.
#' See [kernlab::kpca] for details  and parameters.
#'
#' @examples
#' # Instantiate PipeOpKernelPCA
#' op1 = PipeOpKernelPCA$new()
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpKernelPCA = R6Class("PipeOpKernelPCA",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "pca", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamFct$new("kernel", default = "rbfdot", levels = c("rbfdot", "polydot",
          "vanilladot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")),
        ParamUty$new("kpar"),
        ParamInt$new("features", default = 0, lower = 0),
        ParamDbl$new("th", default = 1e-04, lower = 0),
        ParamUty$new("na.action", default = na.omit)
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "kernlab")
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
    },

    train_dt = function(dt, levels) {
      pcr = invoke(kernlab::kpca, as.matrix(dt), .args = self$param_set$values)
      self$state$pcr = pcr
      kernlab::rotated(pcr)
    },

    predict_dt = function(dt, levels) {
      predict(self$state$pcr, as.matrix(dt))
    }
  )
)

mlr_pipeops$add("kernelpca", PipeOpKernelPCA)


