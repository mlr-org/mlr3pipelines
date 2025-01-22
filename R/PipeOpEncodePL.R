#' @title Piecewise Linear Encoding Base Class
#'
#' @usage NULL
#' @name mlr_pipeops_encodepl
#' @format Abstract [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Abstract base class for piecewise linear encoding.
#'
#' Piecewise linear encoding works by splitting values of features into distinct bins, through an algorithm implemented
#' in `private$.get_bins()`, and then creating new feature columns through a continuous alternative to one-hot encoding.
#' Here, one new feature per bin is constructed, with values being either
#' * `0`, if the original value was below the lower bin boundary,
#' * `1`, if the original value was above the upper bin boundary, or
#' * a scaled value between `0` and `1`, if the original value was inside the bin boundaries. Scaling is done by
#'   offsetting the original value by the lower bin boundary and dividing by the bin width.
#'
#' [`PipeOp`]s inheriting from this encode columns of type `numeric` and `integer`. Use the [`PipeOpTaskPreproc`]
#' `$affect_columns` functionality to only encode a subset of columns, or only encode columns of a certain type, etc.
#'
#' @section Construction:
#' ```
#' PipeOpEncodePL$new(id = "encodepl", param_set = ps(), param_vals = list(), packages = character(0), task_type = "Task)
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`].
#' * `param_set` :: [`ParamSet`][paradox::ParamSet]\cr
#'   Parameter space description. This should be created by the subclass and given to `super$initialize()`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings given in `param_set`. The
#'   subclass should have its own `param_vals` parameter and pass it on to `super$initialize()`. Default `list()`.
#' * `packages` :: `character`\cr
#'   Set of all required packages for the [`PipeOp`]'s `private$.train()` and `private$.predict()` methods. See `$packages` slot.
#'   Default is `character(0)`.
#' * `task_type` :: `character(1)`\cr
#'   The class of [`Task`][mlr3::Task] that should be accepted as input and will be returned as output. This
#'   should generally be a `character(1)` identifying a type of [`Task`][mlr3::Task], e.g. `"Task"`, `"TaskClassif"` or
#'   `"TaskRegr"` (or another subclass introduced by other packages). Default is `"Task"`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreprocSimple`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `numeric` and `integer` columns encoded using piecewise linear encoding.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreprocSimple`], as well as:
#' * `bins` :: named `list`\cr
#'   Named list of numeric vectors. Each element corresponds to one of the affected feature columns and contains the
#'   inner bin boundaries derived through the private method `.get_bins()`. The element vectors are named by the respective
#'   feature column.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`].
#'
#' @section Internals:
#'
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`] as well as
#' * `.get_bins(task, cols)`\cr
#'   ([`Task`][mlr3::Task], `character`) -> named `list` \cr
#'   Abstract method for splitting the value range of a feature column into distinct bins. The argument `cols` should
#'   give the names of the feature columns of the `task` for which bins should be derived. Returns a named list of
#'   numeric vectors containing the bin boundaries for each affected feature column, named by that corresponding feature
#'   column.
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
    initialize = function(id, param_set = ps(), param_vals = list(), packages = character(0), task_type = "Task") {
      super$initialize(id = id, param_set = param_set, param_vals = param_vals, packages = packages,
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
      list(bins = private$.get_bins(task, cols))
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

#' @title Piecewise Linear Encoding using Quantiles
#'
#' @usage NULL
#' @name mlr_pipeops_encodeplquantiles
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpEncodePL`]/[`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes `numeric` and `integer` feature columns using piecewise lienar encoding. For details, see documentation of
#' `PipeOpEncodePL` or the paper referenced below.
#'
#' Bins are constructed by taking the quantiles of the respective feature column as bin boundaries. The number of
#' ... are given by the `numsplits` parameter. CHECK WHETHER THIS WORKS AS INTENDED
#' AND HOW THE NUMBER IS TO BE INTERPRETEED
#' (i.e. numsplits=2 creates 3 values, meaning five bins??!)
#'
#' @section Construction:
#' ```
#' PipeOpEncodePL$new(id = "encodeplquantiles", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encodeplquantiles"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpEncodePL`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `numeric` and `integer` columns encoded using piecewise
#' linear encoding with bins being derived from the quantiles of the respective original feature column..
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpEncodePL`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpEncodePL`]/[`PipeOpTaskPreprocSimple`], as well as:
#' * `numsplits`  :: `numeric(1)` \cr
#'   Default is ``.
#'
#' @section Internals:
#'
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpEncodePL`]/[`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpEncodePL`]/[`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @references
#' `r format_bib("gorishniy_2022")`
#'
#' @family PipeOps
#' @family Piecewise Linear Encoding PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#'
#'
PipeOpEncodePLQuantiles = R6Class("PipeOpEncodePLQuantiles",
  inherit = PipeOpEncodePL,
  public = list(
    initialize = function(id = "encodeplquantiles", param_vals = list()) {
      ps = ps(
        numsplits = p_int(lower = 2, default = 2, tags = c("train", "predict"))
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

#' @title Piecewise Linear Encoding using Decision Trees
#'
#' @usage NULL
#' @name mlr_pipeops_encodepltree
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpEncodePL`] / [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes `numeric` and `integer` feature columns using piecewise lienar encoding. For details, see documentation of
#' `PipeOpEncodePL` or the paper referenced below.
#'
#' Bins are constructed by trainig one decision tree [`Learner`][mlr3::Learner] per feature column, taking the target column into account,
#' and using decision boundaries as bin boundaries.
#'
#' @section Construction:
#' ```
#' PipeOpEncodePLTree$new(task_type, id = "encodepltree", param_vals = list())
#' ```
#' * `task_type` :: `character(1)`\cr
#'   The class of [`Task`][mlr3::Task] that should be accepted as input, given as a `character(1)`. This is used to
#'   construct the appropriate [`Learner`][mlr3::Learner] to be used for obtaining the bins for piecewise linear
#'   encoding. Supported options are `"TaskClassif"`for [`LearnerClassifRpart`][mlr3::LearnerClassifRpart] or
#'   `"TaskRegr"`for [`LearnerRegrRpart`][mlr3::LearnerRegrRpart].
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encodeplquantiles"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpEncodePL`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `numeric` and `integer` columns encoded using piecewise
#' linear encoding with bins being derived from a decision tree [`Learner`][mlr3::Learner] trained on the respective feature column.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpEncodePL`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpEncodePL`], as well as the parameters of
#' the [`Learner`][mlr3::Learner] used for obtaining the bins for piecewise linear encoding.
#'
#' @section Internals:
#'
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpEncodePL`]/[`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpEncodePL`]/[`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @references
#' `r format_bib("gorishniy_2022")`
#'
#' @family PipeOps
#' @family Piecewise Linear Encoding PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' # example code
#'
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

# Registering with "TaskClassif", however both "TaskRegr" and "TaskClassif" are acceptable, see issue ...
mlr_pipeops$add("encodepltree", PipeOpEncodePLTree, list(task_type = "TaskClassif"))
