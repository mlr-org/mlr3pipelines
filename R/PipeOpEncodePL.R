#' @title Factor Encoding
#'
#' @usage NULL
#' @name mlr_pipeops_encode
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes columns of type `numeric` and `integer`.
#'
#'
#'
#' Use the [`PipeOpTaskPreproc`] `$affect_columns` functionality to only encode a subset of columns, or only encode columns of a certain type.
#'
#' @section Construction:
#' ```
#' PipeOpEncodePL$new(task_type, id = "encodepl", param_vals = list())
#' ```
#' * `task_type` :: `character(1)`\cr
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encode"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `numeric` and `integer` columns
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * ` ` :: named `list`\cr
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `method`  :: `character(1)` \cr
#'   Initialized to `""`. One of:
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @references
#' `r format_bib("gorishniy_2022")`
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#'
PipeOpEncodePL = R6Class("PipeOpEncodePL",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(task_type, id = "encodepl", param_vals = list()) {
      # NOTE: Might use different name, change assert, and conditions
      assert_choice(task_type, mlr_reflections$task_types$task)
      if (task_type == "TaskRegr") {
        private$.tree_learner = LearnerRegrRpart$new()
      } else if (task_type == "TaskClassif") {
        private$.tree_learner = LearnerClassifRpart$new()
      } else {
        stopf("Task type %s not supported", task_type)
      }

      private$.encodepl_param_set = ps(
        method = p_fct(levels = c("quantiles", "tree"), tags = c("train", "predict", "required")),
        quantiles_numsplits = p_int(lower = 2, default = 2, tags = c("train", "predict"), depends = quote(method == "quantiles"))
      )
      private$.encodepl_param_set$values = list(method = "quantiles")

      super$initialize(id, param_set = alist(encodepl = private$.encodepl_param_set, private$.tree_learner$param_set),
                       param_vals = param_vals, packages = c("stats", private$.tree_learner$packages),
                       task_type = task_type, tags = "encode", feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .tree_learner = NULL,
    .encodepl_param_set = NULL,

    .get_state = function(task) {
      cols = private$.select_cols(task)
      if (!length(cols)) {
        return(task)  # early exit
      }

      pv = private$.encodepl_param_set$values
      numsplits = pv$quantiles_numsplits %??% 2

      if (pv$method == "quantiles") {
        # TODO: check that min / max is correct here (according to paper / implementation)
        bins = lapply(task$data(cols = cols), function(d) {
          unique(c(min(d), stats::quantile(d, seq(1, numsplits - 1) / numsplits, na.rm = TRUE), max(d)))
        })
      } else {
        learner = private$.tree_learner

        bins = list()
        for (col in cols) {
          t = task$clone(deep = TRUE)$select(col)
          splits = learner$train(t)$model$splits
          # Get column "index" in model splits
          boundaries = unname(sort(splits[, "index"]))

          d = task$data(cols = col)
          bins[[col]] = c(min(d), boundaries, max(d))
        }
      }

      list(bins = bins)
    },

    .transform = function(task) {
      bins = self$state$bins
      cols = names(bins)
      if (!length(cols)) {
        return(task)  # early exit
      }

      dt = task$data(cols = cols)
      res = as.data.table(imap(dt, function(d, col) encode_piecewise_linear(d, col, bins[[col]])))

      task$select(setdiff(task$feature_names, cols))$cbind(res)
    }
  )
)

mlr_pipeops$add("encodepl", PipeOpEncodePL, list(task_type = "TaskRegr"))

# Helper function to implement piecewise linear encoding.
# * column: numeric vector
# * colname: name of `column`
# * bins as numeric vector of boundaries
encode_piecewise_linear = function(column, colname, bins) {
  n_bins = length(bins) - 1

  dt = data.table(matrix(0, length(column), n_bins))
  setnames(dt, paste0(colname, ".bin", seq_len(n_bins)))

  for (t in seq_len(n_bins)) {
    lower = bins[[t]]
    upper = bins[[t + 1]]

    dt[column >= upper, colnames(dt)[[t]] := 1]
    indices = column < upper & column >= lower
    dt[indices, colnames(dt)[[t]] := (column[indices] - lower) / (upper - lower)]
  }

  dt
}
