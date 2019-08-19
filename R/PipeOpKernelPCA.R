#' @title PipeOpKernelPCA
#'
#' @usage NULL
#' @name mlr_pipeops_kernelpca
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Extracts kernel principle components from data. Only affects numerical features.
#' See [kernlab::kpca] for details.
#'
#' @section Construction:
#' ```
#' PipeOpKernelPCA$new(id = "kernelpca", param_vals = list())
#' ```
#" * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"kernelpca"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric parameters replaced by their principal components.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as the returned [`S4`] object of the [`"kpca"` function][kernlab::kpca].
#' The slots of the [`S4`] object can be accessed by accessor function. See [kernlab::kpca].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `kernel` :: `character(1)`\cr
#'   The standard deviations of the principal components. See [`kpca()`][kernlab::kpca].
#' * `kpar` :: `list`\cr
#'   List of hyper-parameters that are used with the kernel function. See [`kpca()`][kernlab::kpca].
#' * `features` :: `numeric(1)`\cr
#'   Number of principal components to return. Default 0 means that all
#'   principal components are returned. See [`kpca()`][kernlab::kpca].
#' * `th` :: `numeric(1)`\cr
#'   The value of eigenvalue under which principal components are ignored. Default is 0.0001. See [`kpca()`][kernlab::kpca].
#' * `na.action` :: `function`\cr
#'   Function to specify NA action. Default is [`na.omit`]. See [`kpca()`][kernlab::kpca].
#'
#' @section Internals:
#' Uses the [`kpca()`][kernlab::kpca] function.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' pop = mlr_pipeops$get("kernelpca")
#'
#' task = mlr3::mlr_tasks$get("iris")
#'
#' task$data()
#'
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
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


