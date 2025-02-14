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
#' * `1`, if the original value was above or equal to the upper bin boundary, or
#' * a scaled value between `0` and `1`, if the original value was inside the bin boundaries. Scaling is done by
#'   offsetting the original value by the lower bin boundary and dividing by the bin width.
#'
#' [`PipeOp`]s inheriting from this encode columns of type `numeric` and `integer`. Use the [`PipeOpTaskPreproc`]
#' `$affect_columns` functionality to only encode a subset of columns, or only encode columns of a certain type, etc.
#'
#' @section Construction:
#' ```
#' PipeOpEncodePL$new(id = "encodepl", param_set = ps(), param_vals = list(), packages = character(0), task_type = "Task")
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
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `numeric` and `integer` columns encoded using piecewise linear encoding.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `bins` :: named `list`\cr
#'   Named list of numeric vectors. Each element corresponds to and is named after one of the affected feature columns
#'   and contains the bin boundaries derived through `private$.get_bins()`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`].
#'
#' @section Internals:
#' `PipeOpEncodePL` is an abstract class inheriting from [`PipeOpTaskPreprocSimple`] that allows easier implementation
#' of different binning algorithms for piecewise linear encoding. The respective binning algorithm should be implemented
#' as `private$.get_bins()`.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
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
        return(list(bins = named_list()))  # early exit
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
      res = imap_dtc(dt, function(d, col) encode_piecewise_linear(d, bins[[col]]))

      task$select(setdiff(task$feature_names, cols))$cbind(res)
    }
  )
)

# Helper function to implement piecewise linear encoding.
# * column: numeric vector
# * bins as numeric vector of boundaries
encode_piecewise_linear = function(column, bins) {
  n_bins = length(bins) - 1

  dt = data.table(matrix(0, length(column), n_bins))
  setnames(dt, paste0("bin", seq_len(n_bins)))

  for (t in seq_len(n_bins)) {
    lower = bins[[t]]
    upper = bins[[t + 1]]
    colname = colnames(dt)[[t]]

    dt[column >= upper, (colname) := 1]
    indices = !is.na(column) & column < upper & column >= lower
    dt[indices, (colname) := (column[indices] - lower) / (upper - lower)]
    # Filling NAs back in
    dt[is.na(column), (colname) := NA]
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
#' Bins are constructed by taking the quantiles of the respective feature column as bin boundaries. The first and
#' last boundaries are set to the minimum and maximum value of the feature, respectively. The number of bins can be
#' controlled with the `numsplits` hyperparameter.
#' Affected feature columns may contain `NA`s. These are ignored when calculating quantiles.
#'
#' @section Construction:
#' ```
#' PipeOpEncodePLQuantiles$new(id = "encodeplquantiles", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encodeplquantiles"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `numeric` and `integer` columns encoded using piecewise
#' linear encoding with bins being derived from the quantiles of the respective original feature column.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpEncodePL`]/[`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `numsplits` :: `integer(1)` \cr
#'   Number of bins to create. Initialized to `2`.
#' * `type` :: `integer(1)`\cr
#'   Method used to calculate sample quantiles. See help of [`stats::quantile`]. Default is `7`.
#'
#' @section Internals:
#' This overloads the `private$.get_bins()` method of [`PipeOpEncodePL`] and uses the [`stats::quantile`] function
#' to derive the bins used for piecewise linear encoding.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpEncodePL`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
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
#' library(mlr3)
#'
#' task = tsk("iris")$select(c("Petal.Width", "Petal.Length"))
#' pop = po("encodeplquantiles")
#'
#' train_out = pop$train(list(task))[[1L]]
#' # Calculated bin boundaries per feature
#' pop$state$bins
#' # Each feature was split into two encoded features using piecewise linear encoding
#' train_out$head()
#'
#' # Prediction works the same as training, using the bins learned during training
#' predict_out = pop$predict(list(task))[[1L]]
#' predict_out$head()
#'
#' # Binning into three bins per feature
#' # Using the nearest even order statistic for caluclating quantiles
#' pop$param_set$set_values(numsplits = 4, type = 3)
#'
#' train_out = pop$train(list(task))[[1L]]
#' # Calculated bin boundaries per feature
#' pop$state$bins
#' # Each feature was split into three encoded features using piecewise linear encoding
#' train_out$head()
PipeOpEncodePLQuantiles = R6Class("PipeOpEncodePLQuantiles",
  inherit = PipeOpEncodePL,
  public = list(
    initialize = function(id = "encodeplquantiles", param_vals = list()) {
      ps = ps(
        numsplits = p_int(lower = 2, init = 2, tags = c("train", "predict", "required")),
        type = p_int(lower = 1, upper = 9, default = 7, tags = c("train", "predict"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "stats")
    }
  ),
  private = list(

    .get_bins = function(task, cols) {
      numsplits = self$param_set$values$numsplits
      # Defaulting to default value in stats::quantile, i.e. method 7
      type = self$param_set$values$type %??% 7

      lapply(task$data(cols = cols), function(d) {
        stats::quantile(d, seq(0, 1, length.out = numsplits + 1), na.rm = TRUE, names = FALSE, type = type)
      })
    }
  )
)

mlr_pipeops$add("encodeplquantiles", PipeOpEncodePLQuantiles)

#' @title Piecewise Linear Encoding using Decision Trees
#'
#' @usage NULL
#' @name mlr_pipeops_encodepltree
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpEncodePL`]/[`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes `numeric` and `integer` feature columns using piecewise lienar encoding. For details, see documentation of
#' `PipeOpEncodePL` or Gorishniy et al. (2022).
#'
#' Bins are constructed by trainig one decision tree [`Learner`][mlr3::Learner] per feature column, taking the target
#' column into account, and using decision boundaries as bin boundaries.
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
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `numeric` and `integer` columns encoded using piecewise
#' linear encoding with bins being derived from a decision tree [`Learner`][mlr3::Learner] trained on the respective feature column.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpEncodePL`]/[`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as the parameters of
#' the [`Learner`][mlr3::Learner] used for obtaining the bins for piecewise linear encoding.
#'
#' @section Internals:
#' This overloads the `private$.get_bins()` method of [`PipeOpEncodePL`]. To derive the bins for each feature, the
#' [`Task`][mlr3::Task] is split into smaller [`Tasks`][mlr3::Task] with only the target and respective feature as columns.
#' On these [`Tasks`][mlr3::Task] either a [`LearnerClassifRpart`][mlr3::LearnerClassifRpart] or
#' [`LearnerRegrRpart`][mlr3::LearnerRegrRpart] gets trained and the respective splits extracted as bin boundaries used
#' for piecewise linear encodings.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpEncodePL`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
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
#' library(mlr3)
#'
#' # For classification task
#' task = tsk("iris")$select(c("Petal.Width", "Petal.Length"))
#' pop = po("encodepltree", task_type = "TaskClassif")
#' train_out = pop$train(list(task))[[1L]]
#'
#' # Calculated bin boundaries per feature
#' pop$state$bins
#' # Each feature was split into three encoded features using piecewise linear encoding
#' train_out$head()
#'
#' # Prediction works the same as training, using the bins learned during training
#' predict_out = pop$predict(list(task))[[1L]]
#' predict_out$head()
#'
#' # Controlling behavior of the tree learner, here: setting minimum number of
#' # observations per node for a split to be attempted
#' pop$param_set$set_values(minsplit = 5)
#'
#' train_out = pop$train(list(task))[[1L]]
#' # feature "hp" now gets split into five encoded features instead of three
#' pop$state$bins
#' train_out$head()
#'
#' # For regression task
#' task = tsk("mtcars")$select(c("cyl", "hp"))
#' pop = po("encodepltree", task_type = "TaskRegr")
#' train_out = pop$train(list(task))[[1L]]
#'
#' # Calculated bin boundaries per feature
#' pop$state$bins
#' # First feature was split into three encoded features, second into two, using piecewise linear encoding
#' train_out$head()
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
        model = learner$train(t)$model
        frame = model$frame

        # Extracting splits according to https://github.com/cran/rpart/blob/983544575b80df286bf8ce238faf4afa145872cd/R/labels.rpart.R#L26
        # NOTE: `frame$nsurrogate` and `frame$ncompete` seem to be always 0 in our case, since competing and surrogate
        # splits only exist when there are more than one feature. Leaving it in for completeness.
        # - surrogates could only be used, if multiple variables exist (cannot use a non-existing variable as surrogate
        #   if the first considered variables has NAs)
        # - competes could not occur with one feature, since no alternative splits are possible (this means alternative
        #   splits with other variables)
        if (nrow(frame) > 1L) {  # do splits exist?
          is_leaf = frame$var == "<leaf>"
          frame = frame[!is_leaf, , drop = FALSE]
          index = cumsum(c(1, frame$nsurrogate + frame$ncompete + 1))
          # remove last entry introduced by prepending 1 in cumsum
          index = index[-length(index)]
          splits = model$splits[index, "index"]
          boundaries = unname(sort(splits))
        } else {
          # No boundaries if no splits exist (only root in frame)
          boundaries = numeric(0)
        }

        d = task$data(cols = col)
        bins[[col]] = c(min(d, na.rm = TRUE), boundaries, max(d, na.rm = TRUE))
      }
      bins
    },

    .additional_phash_input = function() private$.tree_learner$phash
  )
)

# Registering with "TaskClassif", however both "TaskRegr" and "TaskClassif" are acceptable, see #869.
mlr_pipeops$add("encodepltree", PipeOpEncodePLTree, list(task_type = "TaskClassif"))
