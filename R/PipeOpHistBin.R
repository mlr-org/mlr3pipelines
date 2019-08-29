#' @title PipeOpHistBin
#'
#' @usage NULL
#' @name mlr_pipeops_histbin
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Splits numeric features into hist bins.
#'
#' @section Construction:
#' ```
#' PipeOpHistBin$new(id = "histbin", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"histbin"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features replaced by their binded versions.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `bins` :: `list` \cr
#'   List of intervals representing the bins for each numeric feature.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `numsplits`  :: `numeric(1)` \cr
#'   Number of bins to create. Default is \code{2}.
#'
#' @section Internals:
#' Uses the [`stats::hist`] function.
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library(mlr3)
#'
#' task = tsk("iris")
#' pop = po("histbin")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpHistBin = R6Class("PipeOpHistBin",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "histbin", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("breaks", default = "Sturges")
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "graphics")
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
    },

    get_state_dt = function(dt, levels) {
      bins = hist(dt, breaks = self$param_set$values$breaks, plot = FALSE, plot = FALSE)
      list(bins = bins)
    },

    transform_dt = function(dt, levels) {
      .bincode(dt, breaks = self$state$bins)
    }
  )
)

mlr_pipeops$add("histbin", PipeOpHistBin)
