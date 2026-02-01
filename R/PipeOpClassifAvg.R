#' @title Majority Vote Prediction
#'
#' @usage NULL
#' @name mlr_pipeops_classifavg
#' @format [`R6Class`][R6::R6Class] inheriting from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @description
#' Perform (weighted) majority vote prediction from classification [`Prediction`][mlr3::Prediction]s by connecting
#' `PipeOpClassifAvg` to multiple [`PipeOpLearner`] outputs.
#'
#' Always returns a `"prob"` prediction, regardless of the incoming [`Learner`][mlr3::Learner]'s
#' `$predict_type`. The label of the class with the highest predicted probability is selected as the
#' `"response"` prediction. If the [`Learner`][mlr3::Learner]'s `$predict_type` is set to `"prob"`,
#' the probability aggregation is controlled by `prob_aggr` (see below). If `$predict_type = "response"`,
#' predictions are internally converted to one-hot probability vectors (point mass on the predicted class) before aggregation.
#'
#' ### `"prob"` aggregation:
#'
#' * **`prob_aggr = "mean"`** -- *Linear opinion pool (arithmetic mean of probabilities; default)*.
#'   **Interpretation.** Mixture semantics: choose a base model with probability `w[i]`, then draw from its class distribution.
#'   Decision-theoretically, this is the minimizer of `sum(w[i] * KL(p[i] || p))` over probability vectors `p`, where `KL(x || y)` is the Kullback-Leibler divergence.
#'   **Typical behavior.** Conservative / better calibrated and robust to near-zero probabilities (never assigns zero unless all do).
#'   This is the standard choice for probability averaging in ensembles and stacking.
#'
#' * **`prob_aggr = "log"`** -- *Log opinion pool / product of experts (geometric mean in probability space)*:
#'   Average per-model logs (or equivalently, logits) and apply softmax.
#'   **Interpretation.** Product semantics: `p_ens ~ prod_i p_i^{w[i]}`; minimizes `sum(w[i] * KL(p || p[i]))`.
#'   **Typical behavior.** Sharper / lower entropy (emphasizes consensus regions), but can be **overconfident** and is sensitive
#'   to zeros; use `prob_aggr_eps` to clip small probabilities for numerical stability. Often beneficial with strong, similarly
#'   calibrated members (e.g., neural networks), less so when calibration is the priority.
#'
#' All incoming [`Learner`][mlr3::Learner]'s `$predict_type` must agree.
#'
#' Weights can be set as a parameter; if none are provided, defaults to
#' equal weights for each prediction.
#' Defaults to equal weights for each model.
#'
#' @section Construction:
#' ```
#' PipeOpClassifAvg$new(innum = 0, collect_multiplicity = FALSE, id = "classifavg", param_vals = list())
#' ```
#'
#' * `innum` :: `numeric(1)`\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number of inputs.
#' * `collect_multiplicity` :: `logical(1)`\cr
#'   If `TRUE`, the input is a [`Multiplicity`] collecting channel. This means, a
#'   [`Multiplicity`] input, instead of multiple normal inputs, is accepted and the members are aggregated. This requires `innum` to be 0.
#'   Default is `FALSE`.
#' * `id` :: `character(1)`
#'   Identifier of the resulting object, default `"classifavg"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpEnsemble`]. Instead of a [`Prediction`][mlr3::Prediction], a [`PredictionClassif`][mlr3::PredictionClassif]
#' is used as input and output during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpEnsemble`], as well as:
#' * `prob_aggr` :: `character(1)`\cr
#'   Controls how incoming class probabilities are aggregated. One of `"mean"` (linear opinion pool; default) or
#'   `"log"` (log opinion pool / product of experts). See the description above for definitions and interpretation.
#'   Only has an effect if the incoming predictions have `"prob"` values.
#' * `prob_aggr_eps` :: `numeric(1)`\cr
#'   Small positive constant used only for `prob_aggr = "log"` to clamp probabilities before taking logs, improving numerical
#'   stability and avoiding `-Inf`. Ignored for `prob_aggr = "mean"`. Default is `1e-12`.
#'
#' @section Internals:
#' Inherits from [`PipeOpEnsemble`] by implementing the `private$weighted_avg_predictions()` method.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @family Ensembles
#' @template seealso_pipeopslist
#' @include PipeOpEnsemble.R
#' @export
#'
#' @examplesIf requireNamespace("rpart")
#' \donttest{
#' library("mlr3")
#'
#' # Simple Bagging
#' gr = ppl("greplicate",
#'   po("subsample") %>>%
#'   po("learner", lrn("classif.rpart")),
#'   n = 3
#' ) %>>%
#'   po("classifavg")
#'
#' resample(tsk("iris"), GraphLearner$new(gr), rsmp("holdout"))
#' }
PipeOpClassifAvg = R6Class("PipeOpClassifAvg",
  inherit = PipeOpEnsemble,
  public = list(
    initialize = function(innum = 0, collect_multiplicity = FALSE, id = "classifavg", param_vals = list()) {
      param_set = ps(
        prob_aggr = p_fct(levels = c("mean", "log"), init = "mean", tags = c("predict", "prob_aggr")),
        prob_aggr_eps = p_dbl(lower = 0, upper = 1, default = 1e-12, tags = c("predict", "prob_aggr"), depends = quote(prob_aggr == "log"))
      )
      super$initialize(innum, collect_multiplicity, id, param_set = param_set, param_vals = param_vals, prediction_type = "PredictionClassif", packages = "stats")
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights, row_ids, truth) {
      # PredictionClassif makes sure that matrix column names and response levels are identical to levels(x$truth).
      # We therefore only check that truth levels are identical.
      lvls = map(inputs, function(x) levels(x$truth))
      lvls = Reduce(function(x, y) if (identical(x, y)) x else FALSE, lvls)
      if (isFALSE(lvls)) {
        stop("PipeOpClassifAvg input predictions are incompatible (different levels of target variable).")
      }

      prob = NULL
      if (every(inputs, function(x) !is.null(x$prob))) {
        pv = self$param_set$get_values(tags = "prob_aggr")
        if (pv$prob_aggr == "mean") {
          prob = weighted_matrix_sum(map(inputs, "prob"), weights)
        } else {  # prob_aggr == "log"
          epsilon = pv$prob_aggr_eps %??% 1e-12
          prob = weighted_matrix_logpool(map(inputs, "prob"), weights, epsilon = epsilon)
        }
      } else if (every(inputs, function(x) !is.null(x$response))) {
        prob = weighted_factor_mean(map(inputs, "response"), weights, lvls)
      } else {
        stop("PipeOpClassifAvg input predictions had missing 'prob' and missing 'response' values. At least one of them must be given for all predictions.")
      }

      PredictionClassif$new(row_ids = row_ids, truth = truth, prob = pmin(pmax(prob, 0), 1))
    }
  )
)

mlr_pipeops$add("classifavg", PipeOpClassifAvg)
