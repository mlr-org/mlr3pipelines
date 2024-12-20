#' @title Factor Encoding
#'
#' @usage NULL
#' @name mlr_pipeops_encode
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes columns of type `factor` and `ordered`.
#'
#' Use the [`PipeOpTaskPreproc`] `$affect_columns` functionality to only encode a subset of columns, or only encode columns of a certain type.
#'
#' @section Construction:
#' ```
#' PipeOpEncodePL$new(id = "encodepl", param_vals = list())
#' ```
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
    initialize = function(id = "encodepl", param_vals = list()) {
      private$.reg_tree = LearnerRegrRpart$new()

      private$.encodepl_param_set = ps(
        method = p_fct(levels = c("quantiles", "regtree"), tags = c("train", "predict", "required")),
        # cannot set init value for quantiles numsplits since it has depends, use %??% instead? then document it as default or not?
        quantiles_numsplits = p_int(lower = 2, default = 2, tags = c("train", "predict"), depends = quote(method == "quantiles"))
      )
      private$.encodepl_param_set$values = list(method = "quantiles")

      super$initialize(id, param_set = alist(encodepl = private$.encodepl_param_set, private$.reg_tree$param_set),
                       param_vals = param_vals, packages = private$.reg_tree$packages, tags = "encode", feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .reg_tree = NULL,
    .encodepl_param_set = NULL,

    .get_state_dt = function(dt, levels, target) {
      pv = private$.encodepl_param_set$values
      numsplits = pv$quantiles_numsplits %??% 2

      if (pv$method == "quantiles") {
        bins = lapply(dt, function(d)
          unique(c(min(d), stats::quantile(d, seq(1, numsplits - 1) / numsplits, na.rm = TRUE), max(d))))
          # check that min / max is correct here (according to paper / implementation)
      } else {
        learner = private$.reg_tree
        cols = colnames(dt)

        bins = list()
        for (col in cols) {
          t = TaskRegr$new(id = "binning", backend = dt[, ..col], target = task$target_names)
          splits = learner$train(t)$model$splits
          rules = unname(sort(splits[, which(colnames(splits) == "index")]))
          bins[[col]] = c(min(dt[[col]]), rules, max(dt[[col]]))
        }
      }

      list(bins = bins)
    },

    .transform_dt = function(dt, levels) {
      bins = self$state$bins

      cols = colnames(dt)

      for (col in cols) {
        dt = cbind(dt, ple(dt[, ..col], bins[[col]]))
        # do name checking ...
      }

      # Drop old columns
      dt[, (cols) := NULL]
      dt
    }
  )
)

mlr_pipeops$add("encodepl", PipeOpEncodePL)

# Piecewise linear encoding
ple = function(column, bins) {
  n_bins = length(bins) - 1

  dt = data.table(matrix(0, nrow(column), n_bins))
  setnames(dt, paste0(colnames(column), ".bin", seq_len(n_bins)))

  # Transform into vector for logical subsetting in data.table
  vec = column[[1]]

  for (t in seq_len(n_bins)) {
    lower = bins[[t]]
    upper = bins[[t + 1]]

    dt[vec >= upper, colnames(dt)[[t]] := 1]
    indices = vec < upper & vec >= lower
    dt[indices, colnames(dt)[[t]] := (vec[indices] - lower) / (upper - lower)]
  }

  dt
}
