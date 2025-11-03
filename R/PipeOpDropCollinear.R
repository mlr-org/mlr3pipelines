#' @title Drop Collinear Features
#'
#' @usage NULL
#' @name mlr_pipeops_dropcollinear
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Removes numeric features that are almost perfectly linearly dependent on other numeric features by
#' computing a correlation matrix and iteratively discarding columns whose correlations exceed a threshold.
#' By default, features are considered collinear if their absolute correlation is greater than `0.999`,
#' similar to the heuristic used by caret's `findCorrelation()`, and only numeric or integer features are affected.
#'
#' @section Construction:
#' ```
#' PipeOpDropCollinear$new(id = "dropcollinear", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"dropcollinear"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with highly correlated numeric features removed. Remaining
#' columns keep their original order.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `keep` :: `character()`\cr
#'   Names of the feature columns kept after dropping collinear features.
#' * `dropped` :: `character()`\cr
#'   Names of feature columns removed because they exceeded the correlation threshold.
#' * `correlation` :: `matrix` | `NULL`\cr
#'   Correlation matrix of the numeric features seen during training (diagonal set to `NA`), or `NULL`
#'   when less than two numeric features were available.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `threshold` :: `numeric(1)`\cr
#'   Upper bound on the absolute correlation. Columns are iteratively removed until the maximum absolute
#'   correlation is not greater than this value. Initialized to `0.999`.
#' * `method` :: `character(1)`\cr
#'   Correlation method forwarded to [`stats::cor()`], choices `{"pearson", "spearman", "kendall"}`.
#'   Initialized to `"pearson"`.
#' * `use` :: `character(1)`\cr
#'   Handling of missing values as in [`stats::cor()`]; choices `{"everything", "all.obs", "complete.obs",
#'   "na.or.complete", "pairwise.complete.obs"}`. Initialized to `"pairwise.complete.obs"`.
#'
#' @section Internals:
#' Computes the absolute correlation matrix via [`stats::cor()`] on the selected numeric features and
#' repeatedly drops the column with the largest average absolute correlation (breaking ties by the
#' worst maximum correlation and then by column name) until all absolute correlations are below `threshold`.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' task = TaskRegr$new("example",
#'   data = data.table::data.table(
#'     y = rnorm(30),
#'     x1 = seq_len(30),
#'     x2 = 2 * seq_len(30) + 1,
#'     x3 = seq_len(30) + rep(c(0, 1), 15),
#'     f = factor(sample(letters[1:3], 30, replace = TRUE))
#'   ),
#'   target = "y"
#' )
#'
#' pop = po("dropcollinear")
#' transformed = pop$train(list(task))[[1]]
#' transformed$feature_names
#' pop$state$dropped
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpDropCollinear = R6Class("PipeOpDropCollinear",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "dropcollinear", param_vals = list()) {
      ps = ps(
        threshold = p_dbl(lower = 0, upper = 1, tags = c("train", "required", "dropcollinear")),
        method = p_fct(levels = c("pearson", "spearman", "kendall"), tags = c("train", "dropcollinear")),
        use = p_fct(levels = c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"),
          tags = c("train", "dropcollinear"))
      )
      ps$values = list(threshold = 0.999, method = "pearson", use = "pairwise.complete.obs")
      super$initialize(id, param_set = ps, param_vals = param_vals,
        feature_types = c("numeric", "integer"), tags = c("feature selection", "robustify"))
    }
  ),
  private = list(

    .get_state = function(task) {
      cols = private$.select_cols(task)
      if (!length(cols) || task$nrow < 2L) {
        return(list(keep = task$feature_names, dropped = character(0), correlation = NULL))
      }
      params = self$param_set$get_values(tags = "dropcollinear")
      cor_mat = dropcollinear_cor(task$data(cols = cols), params$use, params$method, self$id)
      keep_drop = dropcollinear_select(cor_mat, params$threshold)
      keep = task$feature_names
      if (length(keep_drop$drop)) {
        keep = keep[!(keep %in% keep_drop$drop)]
      }
      list(keep = keep, dropped = keep_drop$drop, correlation = if (length(cols) > 1) dropcollinear_strip_diag(cor_mat) else NULL)
    },

    .transform = function(task) {
      task$select(self$state$keep)
    }
  )
)

mlr_pipeops$add("dropcollinear", PipeOpDropCollinear)

dropcollinear_cor = function(dt, use, method, id) {
  if (!ncol(dt)) {
    return(matrix(numeric(0), nrow = 0, ncol = 0,
      dimnames = list(character(), character())))
  }
  result = tryCatch(
    stats::cor(as.matrix(dt), use = use, method = method),
    error = function(e) stopf("Computing correlations failed in '%s': %s", id, conditionMessage(e))
  )
  if (is.null(colnames(result))) {
    colnames(result) = colnames(dt)
    rownames(result) = colnames(dt)
  }
  result
}

dropcollinear_select = function(cor_mat, threshold) {
  if (!length(cor_mat)) {
    return(list(keep = character(0), drop = character(0)))
  }
  if (ncol(cor_mat) <= 1L) {
    return(list(keep = colnames(cor_mat), drop = character(0)))
  }
  abs_cor = abs(cor_mat)
  abs_cor[is.na(abs_cor)] = 0
  diag(abs_cor) = 0
  cols = colnames(abs_cor)
  drop = character(0)

  while (length(cols) > 1L) {
    max_corr = max(abs_cor)
    if (!is.finite(max_corr) || max_corr <= threshold) {
      break
    }
    mean_corr = rowMeans(abs_cor)
    mean_corr[is.na(mean_corr)] = 0
    max_each = apply(abs_cor, 1L, max)
    max_each[is.na(max_each)] = 0

    worst_idx = which(mean_corr == max(mean_corr))
    if (length(worst_idx) > 1L) {
      worst_idx = worst_idx[max_each[worst_idx] == max(max_each[worst_idx])]
    }
    if (length(worst_idx) > 1L) {
      worst_idx = worst_idx[order(cols[worst_idx], decreasing = TRUE)]
    }
    drop_idx = worst_idx[1L]
    drop = c(drop, cols[drop_idx])

    abs_cor = abs_cor[-drop_idx, -drop_idx, drop = FALSE]
    cols = cols[-drop_idx]
    if (!length(cols)) {
      break
    }
  }

  original_cols = colnames(cor_mat)
  list(keep = original_cols[!(original_cols %in% drop)], drop = drop)
}

dropcollinear_strip_diag = function(mat) {
  if (!length(mat)) {
    return(mat)
  }
  diag(mat) = NA_real_
  mat
}
