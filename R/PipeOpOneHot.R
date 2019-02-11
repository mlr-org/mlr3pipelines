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
#'   If set to "one-hot", creates a new column for each factor level.
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
        ParamFct$new("method", levels = c("one-hot", "treatment", "helmert", "poly", "sum"), default = "one-hot")
      ))
      ps$values = list(method = "one-hot")
      super$initialize(id, param_set = ps, param_vals = param_vals)
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("factor", "ordered", "character"), get("id")]
    },

    get_state_dt = function(dt, levels) {
      contrasts = switch(self$param_set$values$method,
        "one-hot" = function(x) contr.treatment(x, contrasts = FALSE),
        treatment = contr.treatment,
        helmert = contr.helmert,
        poly = function(x) {
          cont = contr.poly(x)
          rownames(cont) = x
          cont
        },
        sum = contr.sum,
        stop("Unknown 'method' parameter value."))
      lapply(levels, contrasts)
    },

    transform_dt = function(dt, levels) {
      cols = imap(self$state, function(contrasts, id) {
        x = dt[[id]]
        contrasts[x, , drop = FALSE]
      })
      cols = as.data.table(cols)
      colnames(cols) = make.names(colnames(cols), unique = TRUE)
      cols
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("onehot", PipeOpOneHot)
