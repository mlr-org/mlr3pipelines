#' @title Ensembling Base Class
#'
#' @usage NULL
#' @format Abstract [`R6Class`][R6::R6Class] inheriting from [`PipeOp`].
#'
#' @description
#' Parent class for [`PipeOp`]s that aggregate predictions. Implements the `private$.train()` and `private$.predict()` methods necessary
#' for a `PipeOp` and requires deriving classes to create the `private$weighted_avg_predictions()` function.
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived class, e.g. [`PipeOpClassifAvg`] or [`PipeOpRegrAvg`].
#' ```
#' PipeOpEnsemble$new(innum = 0, collect_multiplicity = FALSE, id, param_set = ps(), param_vals = list(), packages = character(0), prediction_type = "Prediction")
#' ```
#'
#' * `innum` :: `numeric(1)`\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number of inputs.
#' * `collect_multiplicity` :: `logical(1)`\cr
#'   If `TRUE`, the input is a [`Multiplicity`] collecting channel. This means, a
#'   [`Multiplicity`] input, instead of multiple normal inputs, is accepted and the members are aggregated. This requires `innum` to be 0.
#'   Default is `FALSE`.
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
#'   `private$.train()` and `private$.predict()`.
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
#' [`PipeOpClassifAvg`] and [`PipeOpRegrAvg`] which both inherit from this class.
#'
#' Should it be necessary to use the output of preceding [`Learner`][mlr3::Learner]s
#' during the "training" phase, then [`PipeOpEnsemble`] should not be used. In fact, if training time behaviour of a [`Learner`][mlr3::Learner] is important, then
#' one should use a [`PipeOpLearnerCV`] instead of a [`PipeOpLearner`], and the ensemble can be created with a [`Learner`][mlr3::Learner] encapsulated by a [`PipeOpLearner`].
#' See [`LearnerClassifAvg`] and [`LearnerRegrAvg`] for examples.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`] as well as:
#' * `weighted_avg_prediction(inputs, weights, row_ids, truth)`\cr
#'   (`list` of [`Prediction`][mlr3::Prediction], `numeric`, `integer` | `character`, `list`) -> `NULL`\cr
#'   Create [`Prediction`][mlr3::Prediction]s that correspond to the weighted average of incoming [`Prediction`][mlr3::Prediction]s. This is
#'   called by `private$.predict()` with cleaned and sanity-checked values: `inputs` are guaranteed to fit together,
#'   `row_ids` and `truth` are guaranteed to be the same as each one in `inputs`, and `weights` is guaranteed to have the same length as `inputs`.\cr
#'   This method is abstract, it must be implemented by deriving classes.
#'
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @family Ensembles
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpEnsemble = R6Class("PipeOpEnsemble",
  inherit = PipeOp,
  public = list(
    initialize = function(innum = 0, collect_multiplicity = FALSE, id, param_set = ps(), param_vals = list(), packages = character(0), prediction_type = "Prediction", tags = NULL) {
      assert_integerish(innum, lower = 0)
      if (paradox_info$is_old) {
        paux = ps(weights = p_uty(check_weights(innum), tags = "predict"))
        param_set$add(paux$params$weights)
      } else {
        param_set = c(param_set, ps(weights = p_uty(check_weights(innum), tags = "predict")))
      }
      param_set$values$weights = 1
      inname = if (innum) rep_suffix("input", innum) else "..."
      intype = c("NULL", prediction_type)
      private$.collect = assert_flag(collect_multiplicity)
      if (collect_multiplicity) {
        if (innum) {
          stop("collect_multiplicity only works with innum == 0.")
        }
        inname = "[...]"
        intype = sprintf("[%s]", intype)
      }
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = packages,
        input = data.table(name = inname, train = intype[[1]], predict = intype[[2]]),
        output = data.table(name = "output", train = "NULL", predict = prediction_type),
        tags = c(tags, "ensemble")
      )
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights, row_ids, truth) stop("Abstract."),
    .train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    .predict = function(inputs) {
      if (private$.collect) {
        inputs = unclass(inputs[[1]])
      }
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
    },
    .collect = NULL,
    .additional_phash_input = function() {
      retval = list(private$.collect, self$input$name)
      if (is.null(self$initialize)) return(retval)
      initformals <- names(formals(args(self$initialize)))
      if (!test_subset(initformals, c("id", "param_vals", "innum", "collect_multiplicity", "..."))) {
        warningf("PipeOp %s has construction arguments besides 'id', 'param_vals', 'innum', 'collect_multiplicity' or '...' but .additional_phash_input was not overloaded.

This warning will become an error in the future.", class(self)[[1]], class(self)[[1]])
      }
      retval
    }
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
    function(x) check_numeric(x, any.missing = FALSE)
  } else {
    function(x) {
      tests = c(test_numeric(x, len = innum, any.missing = FALSE), test_numeric(x, len = 1L, any.missing = FALSE))
      if (sum(tests) == 0L) {
        sprintf("Must be of type 'numeric', and length 1 or %s", innum)
      } else {
        TRUE
      }
    }
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

# For a set of n `factor` vectors each of length l with the same k levels and a
# numeric weight vector of length n, returns a matrix of dimension l times k.
# Each cell contains the weighted relative frequency of the respective factor
# level being present at the respective positional index over all n factors.
# If `factors` is a data.frame/data.table and the weights are uniform (1 / l),
# the output would simply be t(apply(factors, 1, table) / l).
# @param factors [`list` of `factor`]: must have the same length and levels
# @param weights [`numeric`]: weights, same length as the list of `factors`
# @return `matrix`
# FIXME: probably should be renamed
weighted_factor_mean = function(factors, weights, alllevels) {
  accmat = matrix(0, nrow = length(factors[[1]]), ncol = length(alllevels))
  colnames(accmat) = alllevels
  for (idx in seq_along(factors)) {
    rdf = data.frame(x = factor(factors[[idx]], levels = alllevels))
    curmat = stats::model.matrix(~ 0 + x, rdf) * weights[idx]
    accmat = accmat + curmat
  }
  accmat
}
