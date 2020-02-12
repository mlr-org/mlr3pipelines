#' @title PipeOpHistBin
#'
#' @usage NULL
#' @name mlr_pipeops_histbin
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Splits numeric features into equally spaced bins.
#' See [graphics::hist()] for details.
#'
#' @section Construction:
#' ```
#' PipeOpHistBin$new(id = "histbin", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"histbin"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features replaced by their binned versions.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `bins` :: `list` \cr
#'   List of intervals representing the bins for each numeric feature.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `bins` :: `character(1)` | `numeric` | `function` \cr
#'   Either a `character(1)` string naming an algorithm to compute the number of cells,
#'   a `numeric(1)` giving the number of breaks for the histogram,
#'   a vector `numeric` giving the breakpoints between the histogram cells, or
#'   a `function` to compute the vector of breakpoints or to compute the number
#'   of cells. Default is algorithm `"Sturges"` (see [`grDevices::nclass.Sturges()`]).
#'   For details see [`hist()`][graphics::hist].
#'
#' @section Internals:
#' Uses the [`graphics::hist`] function.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
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
        ParamUty$new("breaks", default = "Sturges", tags = c("train", "hist"))
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "graphics")
      private$add_tags("feature_type: numeric")
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
    },

    get_state_dt = function(dt, levels, target) {
      bins = lapply(seq_col(dt), function(i) {
        invoke(graphics::hist, dt[[i]], plot = FALSE, .args = self$param_set$get_values(tags = "hist"))$breaks
      })
      list(bins = bins)
    },

    transform_dt = function(dt, levels) {
      as.data.frame(mapply(function(d, b) ordered(cut(d, breaks = b, include.lowest = TRUE)),
        d = dt, b = self$state$bins, SIMPLIFY = FALSE), row.names = rownames(dt))
    }
  )
)

mlr_pipeops$add("histbin", PipeOpHistBin)
