
#' @title PipeOpBoxCox
#'
#' @usage NULL
#' @name mlr_pipeops_boxcox
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Splits numeric features into equally spaced bins.
#' See [graphics::hist()] for details.
#'
#' @section Construction:
#' ```
#' PipeOpBoxCox$new(id = "boxcox", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"boxcox"`.
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
#' * `bins`  :: `character(1)|numeric|function` \cr
#'   Either a character string naming an algorithm to compute the number of cells,
#'   a single number giving the number of breaks for the histogram,
#'   a vector of numbers giving the breakpoints between the histogram cells or
#'   a function to compute the vector of breakpoints or to compute the number
#'   of cells. Default is algorithm "Sturges" (see grDevices::nclass.Sturges()])
#'   For details see [`hist()`][graphics::hist].
#'
#' @section Internals:
#' Uses the [`graphics::hist`] function.
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library(mlr3)
#'
#' task = tsk("iris")
#' pop = po("boxcox")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpBoxCox = R6Class("PipeOpBoxCox",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "boxcox", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("formula"),
        ParamUty$new("lambda", default = seq(-2, 2, 0.1), tags = "train"),
        ParamLgl$new("interp", default = TRUE),
        ParamDbl$new("eps", default = 0.02)
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "smotefamily")
    },
    train_task = function(task) {
      #assert_true(all(task$feature_types$type == "numeric"))
      dt_columns = self$select_cols(task)
      cols = c(task$target_names, dt_columns)
      if (!length(cols)) {
        self$state = list(dt_columns = dt_columns)
        return(task)
      }
      dt = task$data(cols = cols)

      # extract info to use in MASS::boxcox
      ps = self$param_set$values
      formula = ps$formula
      ps$formula = NULL
      ps$object = formula
      ps$data = as.data.frame(dt)
      ps$plotit = FALSE

      # Get best lambda
      bc = invoke(MASS::boxcox, .args = ps)
      max.id = which.max(bc$y)
      lambda = bc$x[max.id]

      # add synthetic data to task data
      target = dt[[1]]
      target = (target ^ lambda - 1)/lambda
      dt[[1]] = target
      task$select(setdiff(c(task$feature_names, task$target_names), cols))$cbind(dt)
    }
  )
)


mlr_pipeops$add("boxcox", PipeOpBoxCox)
