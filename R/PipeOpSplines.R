#' @title Transforms Columns using Splines Methods by Constructing a Model Matrix
#'
#' @usage NULL
#' @name mlr_pipeops_splines
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Transforms Columns using Splines Methods by Constructing a Model Matrix.
#'
#' @section Construction:
#' ```
#' po("basissplines", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"splines"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with the selected columns transformed according to the specified Splines Method.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `factor`  :: `character(1)` \cr
#'   "Polynomial" when polynomial splines are applied [`splines::bs`] or
#'   "natural" when natural natural splines are applied [`splines::bs`].
#'   Default is "polynomial".
#' * `df`  :: `integer(1)` \cr
#'   Number of degrees of freedom for calculation of splines basis matrix.
#'   Default is NULL.
#'
#' @section Internals:
#' For creating the Splines uses the [`splines::bs`]/[`splines::ns`] function.
#' Uses the [`stats::model.matrix()`] function.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("splines")
#'
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export

PipeOpSplines = R6Class("PipeOpSplines",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "splines", param_vals = list()) {
    ps = ps(
      type = p_fct(levels = c("polynomial", "natural"), init = "natural", tags = c("train", "required")),
      df = p_int(lower = 1, upper = Inf, special_vals = list(NULL), init = NULL, tags = "train"),
      degree = p_int(lower = 1, upper = Inf, depends = type == "polynomial", tags = "train")
    )
    super$initialize(id = id, param_set = ps, param_vals = param_vals, packages = c("splines", "stats"))
    }
  ),
  private = list(
    .transform_dt = function(dt, levels) {
      pv = self$param_set$get_values(tags = "train")
      single_string = paste0(if (pv$type == "natural") "splines::ns(dt[[" else "splines::bs(dt[[", seq_along(dt), "]]", if (!is.null(pv$df)) " , df = ", pv$df, if (!is.null(pv$degree)) ", degree = ", pv$degree, ")")
      string = paste(" ~ ", paste(single_string, collapse = " + "))
      result = as.data.frame(stats::model.matrix(formula(string), data = dt))
      max_df = max(regmatches(colnames(result), regexpr("([0-9]+$|$)", colnames(result))))
      if (!is.numeric(max_df)) {
        max_df = 1
      } else {
        max_df = as.numeric(max_df)
      }
      k = 1
      for (j in colnames(dt)) {
        for (i in seq_len(max_df)) {
          colnames(result)[k + 1] = paste0(pv$type, "splines.", j, ".", i)
          k = k + 1
        }
      }
      result
    }
  )
)

mlr_pipeops$add("splines", PipeOpSplines)
