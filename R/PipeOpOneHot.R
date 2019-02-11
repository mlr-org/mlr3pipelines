#' @title PipeOpOneHot
#'
#' @name mlr_pipeop_onehot
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' One-hot encodes all factors
#'
#' Possible parameters are `$frac` and `$ratio`, only one of which must be given. The majority
#' class is never "up"-sampled, so if `$ratio` times the mean minority class size turns out
#' larger than the initial majority class size, the number of rows is shuffled but not changed.
#'
#' @section Parameter Set:
#' * `method`  :: `character(1)` \cr
#'   If set to "1-of-n", creates a new column for each factor level.
#'   If set to "reference", creates $n-1$ columns leaving out the first factor level of each factor variable.
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpOneHot = R6Class("PipeOpOneHot",
  inherit = PipeOpTaskPreprocSimple,

  public = list(
    initialize = function(id = "onehot", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamFct$new("method", levels = c("1-of-n", "reference", "reference-with-intercept"), default = "1-of-n")
        ))
      ps$values = list(method = "1-of-n")
      super$initialize(id, param_set = ps, param_vals = param_vals)
    },

    select_cols = function(task) {
      task$feature_types[get("type") == "factor", get("id")]
    },

    get_state_dt = function(dt) {
      form = switch(self$param_set$values$method,
        "1-of-n" = ~ 0 + .,
        reference = ~ .,
        "reference-with-intercept" = ~ .,
        stop("Unknown 'method' parameter value."))
      attr(form, ".Environment") = baseenv()
      mf = model.frame(form, dt)
      terms = terms(mf)
      if (self$param_set$values$method == "reference") {
        attr(terms, "intercept") = 0L
      }
      list(terms = terms, xlev = .getXlevels(terms(mf), mf))
    },

    transform_dt = function(dt) {
      model.matrix(self$state$terms, dt, xlev = self$state$xlev)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("onehot", PipeOpOneHot)
