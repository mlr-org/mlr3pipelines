#' @title Piecewise Linear Encoding Base Class
#'
#' @usage NULL
#' @name mlr_pipeops_encodepl
#' @format Abstract [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Abstract base class for piecewise linear encoding.
#'
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
#' Methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`], as well as
#' * `.get_bins(task, cols)`\cr
#'   ([`Task`][mlr3::Task], `character`) -> `list` \cr
#'
#'
#' @references
#' `r format_bib("gorishniy_2022")`
#'
#' @family PipeOps
#' @family Piecewise Linear Encoding PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpEncodePL = R6Class("PipeOpEncodePL",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "encodepl", param_set = ps(), param_vals = list()) {
      super$initialize(id, param_set = param_set, param_vals = param_vals,
        task_type = task_type, tags = "encode", feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .get_bins = function(task, cols) {
      stop("Abstract.")
    },

    .get_state = function(task) {
      cols = private$.select_cols(task)
      if (!length(cols)) {
        return(list(bins = numeric(0)))  # early exit
      }
      list(bins = .get_bins(task, cols))
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

#' PipeOpEncodePLQuantiles
PipeOpEncodePLQuantiles = R6Class("PipeOpEncodePLQuantiles",
  inherit = PipeOpEncodePL,
  public = list(
    initialize = function(id = "encodeplquantiles", param_vals = list()) {
      ps = ps(
        numsplits = p_int(lower = 2, default = 2, tags = c("train", "predict", "required"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "stats")
    }
  ),
  private = list(

    .get_bins = function(task, cols) {
      numsplits = self$param_set$values$numsplits %??% 2
      lapply(task$data(cols = cols), function(d) {
        unique(c(min(d), stats::quantile(d, seq(1, numsplits - 1) / numsplits, na.rm = TRUE), max(d)))
      })
    }
  )
)

mlr_pipeops$add("encodeplquantiles", PipeOpEncodePLQuantiles)

#' PipeOpEncodePLTree
PipeOpEncodePLTree = R6Class("PipeOpEncodePLTree",
  inherit = PipeOpEncodePL,
  public = list(
    initialize = function(task_type, id = "encodepltree", param_vals = list()) {
      assert_choice(task_type, mlr_reflections$task_types$task)
      if (task_type == "TaskRegr") {
        private$.tree_learner = LearnerRegrRpart$new()
      } else if (task_type == "TaskClassif") {
        private$.tree_learner = LearnerClassifRpart$new()
      } else {
        stopf("Task type %s not supported.", task_type)
      }

      super$initialize(id, param_set = alist(private$.tree_learner$param_set), param_vals = param_vals,
        packages = private$.tree_learner$packages, task_type = task_type)
    }
  ),
  private = list(

    .tree_learner = NULL,

    .get_bins = function(task, cols) {
      learner = private$.tree_learner

      bins = list()
      for (col in cols) {
        t = task$clone(deep = TRUE)$select(col)
        # Get column "index" in model splits
        boundaries = unname(sort(learner$train(t)$model$splits[, "index"]))
        d = task$data(cols = col)
        bins[[col]] = c(min(d), boundaries, max(d))
      }
      bins
    }
  )
)

# Registering with "TaskRegr", however both "TaskRegr" and "TaskClassif" are acceptable, see issue ...
mlr_pipeops$add("encodepltree", PipeOpEncodePLTree, list(task_type = "TaskRegr"))
