#' @title PipeOpEnsemble
#'
#' @usage NULL
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Parent class for [`PipeOp`]s that aggregate predictions. Implements the `$train_internal()` and `$predict_internal()` methods necessary
#' for a `PipeOp` and requires deriving classes to create the `private$weighted_avg_predictions()` function.
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived class, e.g. [`PipeOpMajorityVote`] or [`PipeOpModelAvg`].
#' ```
#' PipeOpEnsemble$new(innum = 0, id, param_set = ParamSet$new(), param_vals = list(), packages = character(0), prediction_type = "Prediction")
#' ```
#'
#' * `innum` :: `numeric(1)`\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number of inputs.
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting  object.
#' * `param_set` :: [`ParamSet`][paradox::ParamSet]\cr
#'   ("Hyper"-)Parameters in form of a [`ParamSet`][paradox::ParamSet] for the resulting [`PipeOp`].
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#' * `packages` :: `character`\cr
#'   Set of packages required for this `PipeOp`. These packages are loaded during `$train()` and `$predict()`, but not attached.
#'   Default `character(0)`.
#' * `prediction_type` :: `character(1)`\cr
#'   The `predict` entry of the `$input` and `$output` type specifications.
#'   Should be `"Prediction"` (default) or one of its subclasses, e.g. `"PredictionClassif"`, and correspond to the type accepted by
#'   `$train_internal()` and `$predict_internal()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpEnsemble`] has multiple input channels depending on the `innum` construction argument, named `"input1"`, `"input2"`, ...
#' if `innum` is nonzero; if `innum` is 0, there is only one *vararg* input channel named `"..."`.
#' All input channels take only `NULL` during training and take a [`Prediction`][mlr3::Prediction] during prediction.
#'
#' [`PipeOpEnsemble`] has one output channel named `"output"`, producing `NULL` during training and a [`Prediction`][mlr3::Prediction] during prediction.
#'
#' The output during prediction is in some way a weighted averaged representation of the input.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `weights` :: `numeric`\cr
#'   Relative weights of input predictions. If this has length 1, it is ignored and weighs all inputs equally. Otherwise it must have
#'   length equal to the number of connected inputs. Initialized to 1 (equal weights).
#'
#' @section Internals:
#' The commonality of ensemble methods using [`PipeOpEnsemble`] is that they take a `NULL`-input during training and save an empty `$state`. They can be
#' used following a set of [`PipeOpLearner`] [`PipeOp`]s to perform (possibly weighted) prediction averaging. See e.g.
#' [`PipeOpMajorityVote`] and [`PipeOpModelAvg`] which both inherit from this class.
#'
#' Should it be necessary to use the output of preceding [`Learner`][mlr3::Learner]s
#' during the "training" phase, then [`PipeOpEnsemble`] should not be used. In fact, if training time behaviour of a [`Learner`][mlr3::Learner] is important, then
#' one should use a [`PipeOpLearnerCV`] instead of a [`PipeOpLearner`], and the ensembling can be done by a [`Learner`][mlr3::Learner] encapsuled by a [`PipeOpLearner`].
#' See [`LearnerClassifWeightedAverage`] and [`LearnerRegrWeightedAverage`] for examples.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`] as well as:
#' * `weighted_avg_prediction(inputs, weights, row_ids, truth)`\cr
#'   (`list` of [`Prediction`][mlr3::Prediction], `numeric`, `integer` | `character`, `list`) -> `NULL`\cr
#'   Create [`Prediction`][mlr3::Prediction]s that correspond to the weighted average of incoming [`Prediction`][mlr3::Prediction]s. This is
#'   called by `$predict_internal()` with cleaned and sanity-checked values: `inputs` are guaranteed to fit together,
#'   `row_ids` and `truth` are guaranteed to be the same as each one in `inputs`, and `weights` is guaranteed to have the same length as `inputs`.\cr
#'   This method is abstract, it must be implemented by deriving classes.
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpEnsemble = R6Class("PipeOpEnsemble",
  inherit = PipeOp,
  public = list(
    initialize = function(innum = 0, id, param_set = ParamSet$new(), param_vals = list(), packages = character(0), prediction_type = "Prediction") {
      assert_integerish(innum, lower = 0)
      param_set$add(ParamUty$new("weights", custom_check = check_weights(innum)))
      param_set$values$weights = 1
      inname = if (innum) rep_suffix("input", innum) else "..."
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = packages,
        input = data.table(name = inname, train = "NULL", predict = prediction_type),
        output = data.table(name = "output", train = "NULL", predict = prediction_type))
    },
    train_internal = function(inputs) {
      self$state = list()
      list(NULL)
    },
    predict_internal = function(inputs) {
      weights = self$param_set$values$weights
      row_ids = inputs[[1]]$row_ids
      map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
      truth = inputs[[1]]$truth
      if (length(weights) == 1) weights = rep(1, length(inputs))
      weights = weights / sum(weights)
      assert_numeric(weights, any.missing = FALSE, len = length(inputs))

      # Drop zero-weights for efficiency
      # FIXME: this is not numerically stable
      # Note: this is the behaviour of stats:::weighted.mean.default
      inputs = inputs[weights != 0]
      weights = weights[weights != 0]

      list(private$weighted_avg_predictions(inputs, weights, row_ids, truth))
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights, row_ids, truth) stop("Abstract.")
  )
)

# Check function for ParamUty: Check that "weight" parameter
# is a numeric vector and
# has either length 1 or length `innum`. `innum` can be 0 (vararg),
# in which case any length is accepted.
#
# It is necessary to put this function in top level because ParamUty does not
# handle function environments well.
check_weights = function(innum) {
  if (innum == 0) {
    function(x) assert_numeric(x, any.missing = FALSE)
  } else {
    function(x)
      assert(check_numeric(x, len = innum, any.missing = FALSE),
        check_numeric(x, len = 1, any.missing = FALSE))
  }
}

# Weighted sum of `matrices`
# @param matrices [`list` of `matrix`]: matrices to sum up, must be the same shape
# @param weights [`numeric`]: weights, same length as `matrices`
# @return `matrix`
weighted_matrix_sum = function(matrices, weights) {
  accmat = matrices[[1]] * weights[1]
  for (idx in seq_along(matrices)[-1]) {
    accmat = accmat + matrices[[idx]] * weights[idx]
  }
  accmat
}

# Weighted mode of factors: For a set of `factor` vectors, gives
# a vector with the same length, for each position the level with
# the highest total weight, ties broken at random.
# @param factors [`list` of `factor`]: must have the same length and levels
# @param weights [`numeric`]: weights, same length as `factors`
# @return `factor`
weighted_factor_mean = function(factors, weights, alllevels) {
  accmat = matrix(0, nrow = length(factors[[1]]), ncol = length(alllevels))
  for (idx in seq_along(factors)) {
    rdf = data.frame(x = factor(factors[[idx]], levels = alllevels))
    curmat = stats::model.matrix(~ 0 + x, rdf) * weights[idx]
    accmat = accmat + curmat
  }
  factor(alllevels[max.col(accmat)], levels = alllevels)
}
