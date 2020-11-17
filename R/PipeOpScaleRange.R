#' @title PipeOpScaleRange
#'
#' @usage NULL
#' @name mlr_pipeops_scalerange
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Linearly transforms numeric data columns so they are between `lower`
#' and `upper`. The formula for this is \eqn{x' = offset + x * scale},
#' where \eqn{scale} is \eqn{(upper - lower) / (max(x) - min(x))} and
#' \eqn{offset} is \eqn{-min(x) * scale + lower}. The same transformation is applied during training and
#' prediction.
#'
#' @section Construction:
#' ```
#' PipeOpScaleRange$new(id = "scalerange", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"scalerange"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with scaled numeric features.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`],
#' as well as the two transformation parameters \eqn{scale} and \eqn{offset} for each numeric
#' feature.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `lower` :: `numeric(1)`\cr
#'   Target value of smallest item of input data. Initialized to 0.
#' * `upper` :: `numeric(1)`\cr
#'   Target value of greatest item of input data. Initialized to 1.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("scalerange", param_vals = list(lower = -1, upper = 1))
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpScaleRange = R6Class("PipeOpScaleRange",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "scalerange", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("lower", tags = c("required", "train")),
        ParamDbl$new("upper", tags = c("required", "train"))
      ))
      ps$values = list(lower = 0, upper = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      lapply(dt, function(x) {
        rng = range(x, na.rm = TRUE, finite = TRUE)
        scale = (self$param_set$values$upper - self$param_set$values$lower) / diff(rng)
        offset = -rng[1L] * scale + self$param_set$values$lower
        c(scale = scale, offset = offset)
      })
    },

    .transform_dt = function(dt, levels) {
      for (i in seq_along(dt)) {
        trafo = self$state[[i]]
        dt[[i]] = trafo[["offset"]] + dt[[i]] * trafo[["scale"]]
      }
      dt
    }
  )
)

mlr_pipeops$add("scalerange", PipeOpScaleRange)
