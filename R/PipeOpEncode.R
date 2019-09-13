#' @title PipeOpEncode
#'
#' @usage NULL
#' @name mlr_pipeops_encode
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes columns of type `factor`, `character` and `ordered`.
#'
#' Possible encodings are `"one-hot"` encoding, as well as encoding according to `stats::contr.helmert()`, `stats::contr.poly()`,
#' `stats::contr.sum()` and `stats::contr.treatment()`.
#' Newly created columns are named via pattern `[column-name].[x]` where `x` is the respective factor level for `"one-hot"` and
#' `"treatment"` encoding, and an integer sequence otherwise.
#'
#' Use the [`PipeOpTaskPreproc`] `$affect_columns` functionality to only encode a subset of columns, or only encode columns of a certain type.
#'
#' @section Construction:
#' ```
#' PipeOpEncode$new(id = "encode", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encode"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `factor`, `character` or `ordered` parameters encoded according to the `method`
#' parameter.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `constrasts` :: named `list` of `matrix`\cr
#'   List of contrast matrices, one for each affected discrete feature. The rows of each matrix correspond to (training task) levels, the the
#'   columns to the new columns that replace the old discrete feature. See [`stats::contrasts`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `method`  :: `character(1)` \cr
#'   Initialized to `"one-hot"`. One of:
#'   * `"one-hot"`: create a new column for each factor level.
#'   * `"treatment"`: create $n-1$ columns leaving out the first factor level of each factor variable (see `stats::contr.treatment()`).
#'   * `"helmert"`: create columns according to Helmert contrasts (see `stats::contr.helmert()`).
#'   * `"poly"`: create columns with contrasts based on orthogonal polynomials (see `stats::contr.poly()`).
#'   * `"sum"`: create columns with contrasts summing to zero, (see `stats::contr.sum()`).
#'
#' @section Internals:
#' Uses the [`stats::contrasts`] functions. This is relatively inefficient for features with a large number of levels.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library(mlr3)
#'
#' task = TaskClassif$new("task",
#'   data.table::data.table(x = letters[1:3], y = letters[1:3]), "x")
#'
#' poe = po("encode")
#'
#' # poe is initialized with encoding: "one-hot"
#' poe$train(list(task))[[1]]$data()
#'
#' # other kinds of encoding:
#' poe$param_set$values$method = "treatment"
#' poe$train(list(task))[[1]]$data()
#'
#' poe$param_set$values$method = "helmert"
#' poe$train(list(task))[[1]]$data()
#'
#' poe$param_set$values$method = "poly"
#' poe$train(list(task))[[1]]$data()
#'
#' poe$param_set$values$method = "sum"
#' poe$train(list(task))[[1]]$data()
PipeOpEncode = R6Class("PipeOpEncode",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "encode", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamFct$new("method", levels = c("one-hot", "treatment", "helmert", "poly", "sum"), tags = c("train", "predict"))
      ))
      ps$values = list(method = "one-hot")
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "stats")
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("factor", "ordered", "character"), get("id")]
    },

    get_state_dt = function(dt, levels, target) {
      contrasts = switch(self$param_set$values$method,
        "one-hot" = function(x) stats::contr.treatment(x, contrasts = FALSE),
        treatment = stats::contr.treatment,
        helmert = stats::contr.helmert,
        poly = function(x) {
          cont = stats::contr.poly(x)
          rownames(cont) = x
          colnames(cont) = NULL
          cont
        },
        sum = stats::contr.sum,
        stop("Unknown 'method' parameter value.")
      )
      list(contrasts = lapply(levels, function(x) {
        con = contrasts(x)
        if (is.null(colnames(con))) {
          colnames(con) = as.character(seq_len(ncol(con)))
        }
        con
      }))
    },

    transform_dt = function(dt, levels) {
      cols = imap(self$state$contrasts, function(contrasts, id) {
        x = dt[[id]]
        contrasts[as.character(x), , drop = FALSE]
      })
      cols = as.data.table(cols)
      setnames(cols, names(cols), make.names(names(cols), unique = TRUE))
      cols
    }
  )
)

mlr_pipeops$add("encode", PipeOpEncode)
