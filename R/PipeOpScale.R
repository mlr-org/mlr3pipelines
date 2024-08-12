#' @title Center and Scale Numeric Features
#'
#' @usage NULL
#' @name mlr_pipeops_scale
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
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
#' * `id` :: `character(1)`\cr
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
#'   The mean / median (depending on `robust`) of each numeric feature during training, or 0 if `center` is `FALSE`. Will be subtracted during the predict phase.
#' * `scale` :: `numeric`\cr
#'   The value by which features are divided. 1 if `scale` is `FALSE`\cr
#'   If `robust` is `FALSE`, this is the root mean square, defined as `sqrt(sum(x^2)/(length(x)-1))`, of each feature, possibly after centering.
#'   If `robust` is `TRUE`, this is the mean absolute deviation multiplied by 1.4826 (see [stats::mad] of each feature, possibly after centering.
#'   This is 1 for features that are constant during training if `center` is `TRUE`, to avoid division-by-zero.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `center` :: `logical(1)`\cr
#'   Whether to center  features, i.e. subtract their `mean()` from them. Default `TRUE`.
#' * `scale` :: `logical(1)`\cr
#'   Whether to scale features, i.e. divide them by `sqrt(sum(x^2)/(length(x)-1))`. Default `TRUE`.
#' * `robust` :: `logical(1)`\cr
#'   Whether to use robust scaling; instead of scaling / centering with mean / standard deviation,
#'   median and median absolute deviation [`mad`][stats::mad] are used.
#'   Initialized to `FALSE`.
#'
#' @section Internals:
#' Imitates the [`scale()`][base::scale] function for `robust = FALSE` and alternatively subtracts the
#' `median` and divides by [`mad`][stats::mad] for `robust = TRUE`.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pos = po("scale")
#'
#' pos$train(list(task))[[1]]$data()
#'
#' one_line_of_iris = task$filter(13)
#'
#' one_line_of_iris$data()
#'
#' pos$predict(list(one_line_of_iris))[[1]]$data()
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpScale = R6Class("PipeOpScale",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "scale", param_vals = list()) {
      ps = ps(
        center = p_lgl(default = TRUE, tags = c("train", "scale")),
        scale = p_lgl(default = TRUE, tags = c("train", "scale")),
        robust = p_lgl(tags = c("train", "required"))
      )
      ps$values = list(robust = FALSE)
      super$initialize(id = id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .train_dt = function(dt, levels, target) {
      pv = self$param_set$get_values(tags = "train")
      cnames = colnames(dt)
      if (pv$center %??% TRUE) {
        meaning = if (pv$robust) stats::median else mean
        center = map_dbl(dt, meaning, na.rm = TRUE)
      } else {
        center = rep(0, ncol(dt))
      }
      dt = as.data.table(pmap(list(dt, center), `-`))
      setnames(dt, cnames)
      if (pv$scale %??% TRUE) {
        if (pv$robust) {
          scale = map_dbl(dt, stats::mad, na.rm = TRUE, center = 0)
        } else {
          scale = map_dbl(dt, function(feat) {
            not_na = sum(!is.na(feat))
            if (not_na == 0) return(1)
            sqrt(sum(feat^2, na.rm = TRUE) / max(not_na - 1, 1))
          })
        }
        scale[scale == 0] = 1
      } else {
        scale = rep(1, ncol(dt))
      }
      dt = as.data.table(pmap(list(dt, scale), `/`))
      setnames(dt, cnames)
      self$state = list(center = center, scale = scale)
      dt
    },

    .predict_dt = function(dt, levels) {
      t((t(dt) - self$state$center) / self$state$scale)
    }
  )
)

mlr_pipeops$add("scale", PipeOpScale)
