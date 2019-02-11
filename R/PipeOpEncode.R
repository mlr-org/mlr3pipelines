#' @title PipeOpEncode
#'
#' @name mlr_pipeop_encode
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Encodes `factor`, `character` and `ordered factor` columns.
#'
#' Possible encodings are `"one-hot"` encoding, as well as encoding according to `stats::contr.helmert()`, `stats::contr.poly()`,
#' `stats::contr.sum()` and `stats::contr.treatment()`. Newly created columns are named by factor levels in `"one-hot"` and
#' `"treatment"` encoding, and numbered otherwise.
#'
#' Use the `$affect_columns` functionality to only encode a subset of columns, or only encode columns of a certain type.
#'
#' @section Parameter Set:
#' * `method`  :: `character(1)` \cr
#'   If set to `"one-hot"`, creates a new column for each factor level. If set to `"treatment"`, creates $n-1$ columns leaving
#'   out the first factor level of each factor variable (see `stats::contr.treatment()`). If set to `"helmert"`, creates
#'   columns according to Helmert contrasts (see `stats::contr.helmert()`). If set to `"poly"`, creates columns with
#'   contrasts based on orthogonal polynomials (see `stats::contr.poly()`). If set to `"sum"`, creates columns with contrasts
#'   summing to zero, (see `stats::contr.sum()`).
#'   `
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpEncode = R6Class("PipeOpEncode",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "encode", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamFct$new("method", levels = c("one-hot", "treatment", "helmert", "poly", "sum"), default = "one-hot")
      ))
      ps$values = list(method = "one-hot")
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "stats")
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("factor", "ordered", "character"), get("id")]
    },

    get_state_dt = function(dt, levels) {
      contrasts = switch(self$param_set$values$method,
        "one-hot" = function(x) stats::contr.treatment(x, contrasts = FALSE),
        treatment = stats::contr.treatment,
        helmert = stats::contr.helmert,
        poly = function(x) {
          cont = stats::contr.poly(x)
          rownames(cont) = x
          cont
        },
        sum = stats::contr.sum,
        stop("Unknown 'method' parameter value."))
      lapply(levels, function(x) {
        con = contrasts(x)
        if (is.null(colnames(con))) {
          colnames(con) = as.character(seq_len(ncol(con)))
        }
        con
      })
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
mlr_pipeops$add("encode", PipeOpEncode)
