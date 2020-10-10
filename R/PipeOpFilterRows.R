#' @title PipeOpFilterRows
#'
#' @usage NULL
#' @name mlr_pipeops_filterrows
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Filter rows of the data of a [`Task`][mlr3::Task].
#' Also directly allows for the removal of rows with missing values with respect to some user-defined features.
#' If both row filtering and missing value removal is performed, filtering is done after missing value removal.
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
#' The output is the input [`Task`][mlr3::Task] with rows kept according to the filtering expression and
#' rows with missing values with respect to the user-defined features removed.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `na_selection` :: `character` \cr
#'   A `character` vector of all feature names that are checked for missing values in the [`Task`][mlr3::Task].
#'   Initialized to [`selector_none()`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `filter_formula` :: `NULL` | `formula` \cr
#'   Expression of the filtering to be performed, in the form of a `formula` that evaluates to `TRUE` or `FALSE`
#'   for each row within the data of the [`Task`][mlr3::Task].
#'   Rows for which the evaluation is `TRUE` are kept, others are removed.
#'   Initialized to `NULL`, i.e., no filtering is performed and all rows are kept.
#' * `na_selector` :: `function` | [`Selector`] \cr
#'   [`Selector`] function, takes a [`Task`][mlr3::Task] as an argument and returns a `character` vector of features
#'   to check for missing values.
#'   Rows with missing values with respect to these features are removed.
#'   See [`Selector`] for example functions.
#'   Initialized to `selector_none()`, i.e., no missing value removal is performed.
#'
#' @section Internals:
#' A `formula` created using the `~` operator always contains a reference to the `environment` in which
#' the `formula` is created. This makes it possible to use variables in the `~`-expressions that both
#' reference either column names or variable names.
#'
#' Uses the [`is.na()`][base::is.na] function for the checking of missing values.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' task = tsk("pima")
#' po = PipeOpFilterRows$new(param_vals = list(
#'   filter_formula = ~ age < 31 & glucose > median(glucose),
#'   na_selector = selector_all())
#' )
#' po$train(list(task))
#' po$state
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpFilterRows = R6Class("PipeOpFilterRows",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "filterrows", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("filter_formula", tags = c("train", "predict"), custom_check = check_filter_formulae),
        ParamUty$new("na_selector", tags = c("train", "required"), custom_check = check_function)
      ))
      ps$values = list(filter_formula = NULL, na_selector = selector_none())
      super$initialize(id, param_set = ps, param_vals = param_vals)
    }
  ),
  private = list(
    .get_state = function(task) {
      na_selection = self$param_set$values$na_selector(task)
      assert_subset(na_selection, task$feature_names)
      list(na_selection = na_selection)
    },

    .transform = function(task) {
      row_ids = task$row_ids

      na_ids = if (length(self$state$na_selection)) {
        row_ids[which(rowSums(is.na(task$data(cols = self$state$na_selection))) > 0L)]
      } else {
        integer(0L)
      }
      row_ids = setdiff(row_ids, na_ids)

      if (length(self$param_set$values$filter_formula)) {
        frm = self$param_set$values$filter_formula
        row_ids = row_ids[which(eval(frm[[2L]], envir = task$data(row_ids, cols = task$feature_names), enclos = environment(frm)))]
      }

      task$filter(row_ids)
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

mlr_pipeops$add("filterrows", PipeOpFilterRows)
