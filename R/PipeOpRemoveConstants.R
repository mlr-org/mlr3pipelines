#' @title Remove Constant Features
#'
#' @usage NULL
#' @name mlr_pipeops_removeconstants
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Remove constant features from a [mlr3::Task].
#' For each feature, calculates the ratio of features which differ from their mode value.
#' All features with a ratio below a settable threshold are removed from the task.
#' Missing values can be ignored or treated as a regular value distinct from non-missing values.
#'
#' @section Construction:
#' ```
#' PipeOpRemoveConstants$new(id = "removeconstants")
#' ```
#'
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, defaulting to `"removeconstants"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section State:
#' `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `features` :: `character()`\cr
#'   Names of features that are being kept. Features of types that the [`Filter`][mlr3filters::Filter] can not operate on are always being kept.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpTaskPreproc`], as well as:
#' * `ratio` :: `numeric(1)`\cr
#'   Ratio of values which must be different from the mode value in order to keep a feature in the task.
#'   Initialized to 0, which means only constant features with exactly one observed level are removed.
#' * `rel_tol` :: `numeric(1)`\cr
#'   Relative tolerance within which to consider a numeric feature constant. Set to 0 to disregard relative tolerance. Initialized to `1e-8`.
#' * `abs_tol` :: `numeric(1)`\cr
#'   Absolute tolerance within which to consider a numeric feature constant. Set to 0 to disregard absolute tolerance. Initialized to `1e-8`.
#' * `na_ignore` :: `logical(1)`\cr
#'   If `TRUE`, the ratio is calculated after removing all missing values first, so a column can be "constant" even if some but not all values are `NA`.
#'   Initialized to `TRUE`.
#'
#' @section Fields:
#' Fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#' data = data.table::data.table(y = runif(10), a = 1:10, b = rep(1, 10), c = rep(1:2, each = 5))
#'
#' task = TaskRegr$new("example", data, target = "y")
#'
#' po = po("removeconstants")
#'
#' po$train(list(task = task))[[1]]$data()
#'
#' po$state
PipeOpRemoveConstants = R6Class("PipeOpRemoveConstants",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "removeconstants", param_vals = list()) {
      ps = ps(
          ratio = p_dbl(lower = 0, upper = 1, tags = c("train", "required", "constant_check")),
          rel_tol = p_dbl(lower = 0, tags = c("required", "constant_check", "train")),
          abs_tol = p_dbl(lower = 0, tags = c("required", "constant_check", "train")),
          na_ignore = p_lgl(tags = c("train", "required", "constant_check"))
      )
      ps$values = list(ratio = 0, rel_tol = 1e-8, abs_tol = 1e-8, na_ignore = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals, tags = "robustify")
    }
  ),
  private = list(

    .get_state = function(task) {
      pv = self$param_set$get_values(tags = "constant_check")
      list(features = names(invoke(discard, as.list(task$data(cols = task$feature_names)),
        is_constant_enough, .args = pv)))
    },

    .transform = function(task) {
      task$select(self$state$features)
    }
  )
)

mlr_pipeops$add("removeconstants", PipeOpRemoveConstants)

is_constant_enough = function(x, ratio, rel_tol, abs_tol, na_ignore) {
  x[is.nan(x)] = NA
  if (na_ignore) {
    x = x[!is.na(x)]
  }
  if (!length(x)) return(TRUE)
  required_size = length(x) - floor(length(x) * ratio)
  if (required_size <= 1) return(TRUE)

  if (is.numeric(x)) {
    # consider non-finite values first (as if they are distinct)
    # note that 'NA' is not 'finite' and not 'infinite'.
    x_nonf = x[!is.finite(x)]
    if (length(x_nonf)) {
      tbl = as.data.table(x_nonf)[, .N, by = list(x_nonf)]
      if (max(tbl$N) >= required_size) return(TRUE)
    }

    # now consider finite values: sort them and see if items that are
    # 'required_size - 1' steps away from each other differ by at most 'abs_tol' or 'rel_tol'
    x = sort(x[is.finite(x)])
    if (length(x) < required_size) {
      return(FALSE)
    }
    first_x = x[seq.int(1, length(x) - required_size + 1)]
    last_x = x[seq.int(required_size, length(x))]

    # check both abs and rel difference
    any(abs(first_x - last_x) <= abs_tol) || any(2 * abs(first_x - last_x) / (abs(first_x) + abs(last_x)) <= rel_tol)
  } else {
    tbl = as.data.table(x)[, .N, by = list(x)]
    # just check if any category is larger than the required category size
    max(tbl$N) >= required_size
  }
}
