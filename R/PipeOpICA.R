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
        # ParamLgl$new("center", default = TRUE),
        # ParamLgl$new("scale.", default = FALSE),
        # ParamInt$new("rank.", default = NULL, lower = 1, upper = Inf, special_vals = list(NULL))
        ParamInt$new("n.comp", default = NULL, lower = 1, upper = Inf, special_vals = list(NULL)),
        ParamFct$new("alg.typ", levels = c("parallel", "deflation"),
          default = "parallel"),
        ParamFct$new("fun", default = "logcosh", levels = c("logcosh", "exp")),
        ParamDbl$new("alpha", default = 1.0, lower = 1, upper = 2),
        ParamFct$new("method", default = "C", levels = c("C", "R")),
        ParamInt$new("maxit", default = 200, lower = 1),
        ParamDbl$new("tol", default = 1e-4, lower = 0),
        ParamLgl$new("verbose", default = FALSE)
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "fastICA")
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
    },

    train_dt = function(dt, levels) {

      if (!ncol(dt)) {
        emat = matrix(data = numeric(0), nrow = 0, ncol = 0)
        control = list(K = emat, W = emat, A = emat, center = numeric(0))
        return(dt)
      }
      if (is.null(self$param_set$values$n.comp)) {
        self$param_set$values$n.comp = ncol(dt)
      }
      ica = invoke(fastICA::fastICA, as.matrix(dt), .args = self$param_set$values)
      ##mlrCPO
      # control = fastICA::fastICA(dt, n.comp = n.comp, alg.typ = alg.typ, fun = fun, alpha = alpha,
      #   method = method, maxit = maxit, tol = tol, verbose = verbose)
      # ret = ica$S
      # ica$S = NULL
      # ica$X = NULL
      # ica$center = vnapply(dt, mean)
      # ret
      self$state = ica
      self$state$S = NULL
      self$state$X = NULL
      self$state$center = vnapply(dt, mean) #tsensemlber
      ica$S
    },

    predict_dt = function(dt, levels) {
      scale(as.matrix(dt), scale = FALSE, center = self$state$center) %*%
        (self$state$K %*% self$state$W)
    }
  )
)

mlr_pipeops$add("pca", PipeOpPCA)
