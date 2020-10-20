#' @title PipeOpFilterRows
#'
#' @usage NULL
#' @name mlr_pipeops_filterrows
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Filter rows of the data of a [`Task`][mlr3::Task].
#'
#' @section Construction:
#' ```
#' PipeOpFilterRows$new(id = "filterrows", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)` \cr
#'   Identifier of resulting object, default `"filterrows"`.
#' * `param_vals` :: named `list` \cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with rows kept according to the filtering expression.
#' Whether filtering is performed during training and/or prediction can be specified via the `phase` parameter, see below.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `filter_formula` :: `NULL` | `formula` \cr
#'   Expression of the filtering to be performed, in the form of a `formula` that evaluates to `TRUE` or `FALSE`
#'   for each row within the frame of the [`data.table`] [`DataBackend`][mlr3::DataBackend] of the [`Task`][mlr3::Task].
#'   Rows for which the evaluation is `TRUE` are kept in the output [`Task`][mlr3::Task], others are removed.
#'   Initialized to `NULL`, i.e., no filtering is performed and all rows are kept.
#' * `SDcols` :: `function` | [`Selector`] \cr
#'   [`Selector`] function, takes a [`Task`][mlr3::Task] as an argument and returns a `character` vector of features.
#'   This character vector is set as the `.SDcols` argument when the formula above is evaluated within the frame of the
#'   [`data.table`] [`DataBackend`][mlr3::DataBackend] of the [`Task`][mlr3::Task].
#'   Initialized to [`selector_all()`], i.e., all features can be used as the `.SD` variable.
#' * `phase` :: `character(1)` \cr
#'   Character specifying the phase when filtering should be performed. Can either be `"always"`, `"train"`, or `"predict"`.
#'   Initialized to `"always"`, i.e., filtering is performed both during training and prediction.
#'
#' @section Internals:
#' A `formula` created using the `~` operator always contains a reference to the `environment` in which
#' the `formula` is created. This makes it possible to use variables in the `~`-expressions that both
#' reference either column names or variable names.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' task = tsk("pima")
#' # filter based on some formula
#' po = PipeOpFilterRows$new(param_vals = list(
#'   filter_formula = ~ age < 31 & glucose > median(glucose, na.rm = TRUE))
#' )
#' po$train(list(task))
#' # missing value removal for all features
#' po$param_set$values$filter_formula = ~ !apply(is.na(.SD), MARGIN = 1L, FUN = any)
#' po$train(list(task))
#' # missing value removal only for some features
#' po$param_set$values$SDcols = selector_name(c("mass", "pressure"))
#' po$train(list(task))
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpFilterRows = R6Class("PipeOpFilterRows",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "filterrows", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("filter_formula", tags = c("train", "predict"), custom_check = check_filter_formulae),
        ParamUty$new("SDcols", tags = c("train", "predict"), custom_check = check_function),
        ParamFct$new("phase", levels = c("always", "train", "predict"), tags = c("train", "predict"))
      ))
      ps$values = list(filter_formula = NULL, SDcols = selector_all(), phase = "always")
      super$initialize(id, param_set = ps, param_vals = param_vals)
    }
  ),
  private = list(
    .train_task = function(task) {
      self$state = list()
      if (self$param_set$values$phase %in% c("always", "train") && length(self$param_set$values$filter_formula)) {
        filter_task(task, frm = self$param_set$values$filter_formula, SDcols = self$param_set$values$SDcols(task))
      } else {
        task
      }
    },

    .predict_task = function(task) {
      if (self$param_set$values$phase %in% c("always", "predict") && length(self$param_set$values$filter_formula)) {
        filter_task(task, frm = self$param_set$values$filter_formula, SDcols = self$param_set$values$SDcols(task))
      } else {
        task
      }
    }
  )
)

# check the `filter_formula` parameter of PipeOpFilterRows
# @param x [formula] whatever `filter_formula` is being set to
# checks that `filter_formula` is `formula` with only a rhs (or NULL)
check_filter_formulae = function(x) {
  check_formula(x, null.ok = TRUE) %check&&%
    if (!is.null(x) && length(x) != 2L) {
      sprintf("formula %s must not have a left hand side.", deparse(x, nlines = 1L, width.cutoff = 500))
    } else {
      TRUE
    }
}

# helper function to filter a task based on a formula
# the formula is evaluated within the frame of the data.table backend of a task where .SDcols is set to SDcols
# (but only if required)
# @param task [Task]
# @param frm [formula]
# @param SDcols [character]
filter_task = function(task, frm, SDcols) {
  row_ids = if (any(grepl(".SD", x = frm[[2L]]))) {
    task$row_ids[which(task$data()[, (eval(frm[[2L]], envir = as.list(environment(frm)))), .SDcols = SDcols])]
  } else {
    task$row_ids[which(task$data()[, (eval(frm[[2L]], envir = as.list(environment(frm))))])]
  }
  task$filter(row_ids)
}

mlr_pipeops$add("filterrows", PipeOpFilterRows)
