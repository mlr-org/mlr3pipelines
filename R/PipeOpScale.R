#' @title PipeOpScale
#'
#' @usage NULL
#' @name mlr_pipeops_scale
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Centers all numeric features to mean = 0 (if `center` parameter is `TRUE`) and scales them
#' by dividing them by their root-mean-square (if `scale` parameter is `TRUE`).
#'
#' The root-mean-square here is defined as `sqrt(sum(x^2)/(length(x)-1))`. If the `center` parameter
#' is `TRUE`, this corresponds to the [`sd()`][stats::sd].
#'
#' @section Construction:
#' ```
#' PipeOpScale$new(id = "scale", param_vals = list())
#' ```
#" * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"scale"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric parameters centered and/or scaled.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `center` :: `numeric`\cr
#'   The mean of each numeric feature during training, or 0 if `center` is `FALSE`. Will be subtracted during the predict phase.
#' * `scale` :: `numeric`\cr
#'   The root mean square, defined as `sqrt(sum(x^2)/(length(x)-1))`, of each feature during training, or 1 if `scale` is FALSE.
#'   During predict phase, feaatures are divided by this.\cr
#'   This is 1 for features that are constant during training if `center` is `TRUE`, to avoid division-by-zero.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `center` :: `logical(1)`\cr
#'   Whether to center  features, i.e. subtract their `mean()` from them. Default `TRUE`.
#' * `scale` :: `logical(1)`\cr
#'   Whether to scale features, i.e. divide them by `sqrt(sum(x^2)/(length(x)-1))`. Default `TRUE`.
#'
#' @section Internals:
#' Uses the [`scale()`][base::scale] function.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' pos = mlr_pipeops$get("scale")
#'
#' pos$train(list("iris"))[[1]]$data()
#'
#' one_line_of_iris = mlr_tasks$get("iris")$filter(13)
#'
#' one_line_of_iris$data()
#'
#' pos$predict(list(one_line_of_iris))[[1]]$data()
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
      constfeat = self$state$scale == 0
      self$state$scale[constfeat] = 1
      sc[, constfeat] = 0
      sc
    },

    predict_dt = function(dt, levels) {
      t((t(dt) - self$state$center) / self$state$scale)
    }
  )
)

mlr_pipeops$add("scale", PipeOpScale)
