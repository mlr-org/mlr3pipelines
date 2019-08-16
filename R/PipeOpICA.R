#' @title PipeOpICA
#'
#' @name mlr_pipeop_ica
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Extract statistically independent components from data by
#' a linear representation of nongaussian data.
#' Use the \code{\link[fastICA]{fastICA}} function implementing the
#' \dQuote{FastICA algorithm}. See the documentation there.
#'
#' @examples
#' # Instantiate PipeOpICA
#' op1 = PipeOpICA$new()
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpICA = R6Class("PipeOpICA",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "ica", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamInt$new("n.comp", lower = 1, upper = Inf),
        ParamFct$new("alg.typ", levels = c("parallel", "deflation"),
          default = "parallel"),
        ParamFct$new("fun", default = "logcosh", levels = c("logcosh", "exp")),
        ParamDbl$new("alpha", default = 1.0, lower = 1, upper = 2),
        ParamFct$new("method", default = "R", levels = c("C", "R")),
        ParamLgl$new("row.norm", default = FALSE),
        ParamInt$new("maxit", default = 200, lower = 1),
        ParamDbl$new("tol", default = 1e-04, lower = 0),
        ParamLgl$new("verbose", default = FALSE),
        ParamUty$new("w.init", default = NULL)
      ))
      ps$values = list(method = "C")
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "fastICA")
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
    },

    train_dt = function(dt, levels) {

      params = insert_named(list(n.comp = ncol(dt)), self$param_set$values)

      ica = invoke(fastICA::fastICA, as.matrix(dt), .args = params)

      self$state = ica
      self$state$S = NULL
      self$state$X = NULL
      self$state$center = map_dbl(dt, mean)
      ica$S
    },

    predict_dt = function(dt, levels) {
      scale(as.matrix(dt), scale = FALSE, center = self$state$center) %*%
        (self$state$K %*% self$state$W)
    }
  )
)

mlr_pipeops$add("ica", PipeOpICA)
