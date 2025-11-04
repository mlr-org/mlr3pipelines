#' @title Weighted Prediction Averaging
#'
#' @usage NULL
#' @name mlr_pipeops_regravg
#' @format [`R6Class`][R6::R6Class] inheriting from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @description
#' Perform (weighted) prediction averaging from regression [`Prediction`][mlr3::Prediction]s by connecting
#' [`PipeOpRegrAvg`] to multiple [`PipeOpLearner`] outputs.
#'
#' The resulting `"response"` prediction is a weighted average of the incoming `"response"` predictions.
#' Aggregation of `"se"` predictions is controlled by the `se_aggr` parameter (see below). When `"se"` is not requested
#' or `se_aggr = "none"`, `"se"` is dropped.
#'
#' @section "se" Aggregation:
#'
#' Let there be `K` incoming predictions with weights `w` (sum to 1). For a given row `j`, denote
#' per-model means `mu_i[j]` and, if available, per-model standard errors `se_i[j]`.
#' Define
#'
#' ```
#' mu_bar[j]      = sum_i w[i] * mu_i[j]
#' var_between[j] = sum_i w[i] * (mu_i[j] - mu_bar[j])^2 # weighted var of means
#' var_within[j]  = sum_i w[i] * se_i[j]^2               # weighted mean of SE^2s
#' ```
#'
#' The following aggregation methods are available:
#'
#' * **`se_aggr = "predictive"`** -- *Within + Between (mixture/predictive SD)*
#'   ```
#'   se[j] = sqrt(var_within[j] + var_between[j])
#'   ```
#'   **Interpretation.** Treats each incoming `se_i` as that model's predictive SD at the point (or, if the learner
#'   reports SE of the conditional mean--as many `mlr3` regression learners do--then as that mean-SE). The returned `se`
#'   is the SD of the *mixture ensemble* under weighted averaging: it increases when base models disagree (epistemic spread)
#'   and when individual models are uncertain (aleatoric spread).
#'   **Notes.** If `se_i` represents *mean* SE (common in `predict.lm(se.fit=TRUE)`-style learners), the result
#'   aggregates those mean-SEs and still adds model disagreement correctly, but it will *underestimate* a true predictive SD
#'   that would additionally include irreducible noise. Requires `"se"` to be present from **all** inputs.
#'
#' * **`se_aggr = "mean"`** -- *SE of the weighted average of means under equicorrelation*
#'   With a correlation parameter `se_aggr_rho = rho`, assume
#'   `Cov(mu_i_hat, mu_j_hat) = rho * se_i * se_j` for all `i != j`. Then
#'   ```
#'   # components:
#'   a[j] = sum_i (w[i]^2 * se_i[j]^2)
#'   b[j] = (sum_i w[i] * se_i[j])^2
#'   var_mean[j] = (1 - rho) * a[j] + rho * b[j]
#'   se[j] = sqrt(var_mean[j])
#'   ```
#'   **Interpretation.** Returns the *standard error of the averaged estimator* `sum_i w[i] * mu_i`, not a predictive SD.
#'   Use when you specifically care about uncertainty of the averaged mean itself.
#'   **Notes.** `rho` is clamped to the PSD range `[-1/(K-1), 1]` for `K > 1`. Typical settings:
#'   `rho = 0` (assume independence; often optimistic for CV/bagging) and `rho = 1` (perfect correlation; conservative and
#'   equal to the weighted arithmetic mean of SEs). Requires `"se"` from **all** inputs.
#'
#' * **`se_aggr = "within"`** -- *Within-model component only*
#'   ```
#'   se[j] = sqrt(var_within[j])
#'   ```
#'   **Interpretation.** Aggregates only the average per-model uncertainty and **ignores** disagreement between models.
#'   Useful as a diagnostic of the aleatoric component; not a full ensemble uncertainty.
#'   **Notes.** Typically *underestimates* the uncertainty of the ensemble prediction when models disagree.
#'   Requires `"se"` from **all** inputs.
#'
#' * **`se_aggr = "between"`** -- *Between-model component only (works without `"se"`)*
#'   ```
#'   se[j] = sqrt(var_between[j])
#'   ```
#'   **Interpretation.** Captures only the spread of the base means (epistemic/model disagreement).
#'   **Notes.** This is the only method that does not use incoming `"se"`. It is a *lower bound* on a full predictive SD,
#'   because it omits within-model noise.
#'
#' * **`se_aggr = "none"`** -- *Do not return `"se"`*
#'   `"se"` is dropped from the output prediction.
#'
#' **Relationships and edge cases.** For any row, `se("predictive") >= max(se("within"), se("between"))`.
#' With a single input (`K = 1`), `"predictive"` and `"within"` return the input `"se"`, `"between"` returns `0`.
#' Methods `"predictive"`, `"mean"`, and `"within"` require all inputs to provide `"se"`; otherwise aggregation errors.
#'
#' Weights can be set as a parameter; if none are provided, defaults to
#' equal weights for each prediction.
#'
#' @section Construction:
#' ```
#' PipeOpRegrAvg$new(innum = 0, collect_multiplicity = FALSE, id = "regravg", param_vals = list())
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
#'   Identifier of the resulting  object, default `"regravg"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpEnsemble`]. Instead of a [`Prediction`][mlr3::Prediction], a [`PredictionRegr`][mlr3::PredictionRegr]
#' is used as input and output during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpEnsemble`], as well as:
#' * `se_aggr` :: `character(1)`\cr
#'   Controls how incoming `"se"` values are aggregated into an ensemble `"se"`. One of
#'   `"predictive"`, `"mean"`, `"within"`, `"between"`, `"none"`. See the description above for definitions and interpretation.
#' * `se_aggr_rho` :: `numeric(1)`\cr
#'   Equicorrelation parameter used only for `se_aggr = "mean"`. Interpreted as the common correlation between
#'   per-model mean estimators. Recommended range `[0, 1]`; values are clamped to `[-1/(K-1), 1]` for validity.
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
#' @examplesIf requireNamespace("rpart")
#' library("mlr3")
#'
#' # Simple Bagging for Regression
#' gr = ppl("greplicate",
#'   po("subsample") %>>%
#'   po("learner", lrn("regr.rpart")),
#'   n = 5
#' ) %>>%
#'   po("regravg")
#'
#' resample(tsk("mtcars"), GraphLearner$new(gr), rsmp("holdout"))
PipeOpRegrAvg = R6Class("PipeOpRegrAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum = 0, collect_multiplicity = FALSE, id = "regravg", param_vals = list(), ...) {
      param_set = ps(
        se_aggr = p_fct(levels = c("predictive", "mean", "within", "between", "none"), init = "none", tags = c("predict", "se_aggr")),
        se_aggr_rho = p_dbl(lower = -1, upper = 1, default = 0, tags = c("predict", "se_aggr"))
      )
      super$initialize(innum, collect_multiplicity, id, param_set = param_set, param_vals = param_vals, prediction_type = "PredictionRegr", ...)
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights, row_ids, truth) {
      responses = map(inputs, "response")
      ses = map(inputs, function(x) if ("se" %in% names(x$data)) x$data$se else NULL)
      if (any(map_lgl(ses, is.null))) {
        ses = NULL
      }
      pv = self$param_set$get_values(tags = "se_aggr")

      response_matrix = simplify2array(responses)
      response = c(response_matrix %*% weights)
      se = aggregate_se_weighted(responses, ses, weights, method = pv$se_aggr, rho = pv$se_aggr_rho %??% 0)

      PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se)
    }
  )
)

mlr_pipeops$add("regravg", PipeOpRegrAvg)


#' Aggregate SEs from multiple learners with weights.
#'
#' @param means_list list of numeric vectors (length N), per-model mean predictions.
#' @param ses_list   NULL or list of numeric vectors (length N), per-model SEs.
#'                   If non-NULL, must have same length and alignment as means_list.
#' @param weights    numeric vector of length K summing to 1 (checked elsewhere).
#' @param method     one of "none", "predictive", "mean", "within", "between".
#' @param rho        numeric scalar for "mean" method; equicorrelation parameter.
#'                   Will be clamped to [-1/(K-1), 1] if K > 1; ignored otherwise.
#' @return numeric vector (length N) of aggregated SEs, or `NULL` if `method = "none"`.
aggregate_se_weighted = function(means_list, ses_list = NULL, weights,
  method = "none",
  rho = 0
) {
  assert_choice(method, c("none", "predictive", "mean", "within", "between"))
  assert_number(rho, lower = -1, upper = 1)
  assert_list(means_list, types = "numeric", any.missing = FALSE)
  assert_list(ses_list, types = "numeric", any.missing = FALSE, len = length(means_list), null.ok = TRUE)
  assert_numeric(weights, len = length(means_list), any.missing = FALSE, finite = TRUE)

  K = length(means_list)
  if (K == 0L) stop("internal error: means_list must have length >= 1.")
  N = length(means_list[[1L]])
  if (!all(vapply(means_list, length, integer(1)) == N)) stop("All mean vectors must have same length.")
  M = do.call(cbind, means_list)                   # N x K matrix of means

  # Precompute weighted mean and between-model variance: Var_w(M) = E_w[M^2] - (E_w[M])^2
  w = as.numeric(weights)
  # normalize defensively (cheap and avoids drift if upstream check is skipped)
  sw = sum(w)
  if (!isTRUE(all.equal(sw, 1))) w = w / sw

  mu_bar = drop(M %*% w)                            # length N
  Ew_M2  = rowSums(M * M * rep(w, each = N))        # E_w[M^2]
  v_between = pmax(Ew_M2 - mu_bar^2, 0)             # numerical guard

  if (method == "between") {
    return(sqrt(v_between))
  }

  if (method == "none") {
    return(NULL)
  }

  if (is.null(ses_list)) {
    stop("Selected method requires `ses_list`, but it is NULL. Use method \"between\" or \"none\".")
  }
  if (length(ses_list) != K) stop("ses_list length must equal means_list length.")
  if (!all(vapply(ses_list, length, integer(1)) == N)) stop("All SE vectors must have same length.")

  S = do.call(cbind, ses_list)                      # N x K matrix of SEs
  S2w  = rowSums((S * S) * rep(w,  each = N))       # sum_i w_i s_i^2 (within term)

  if (method == "within") {
    return(sqrt(pmax(S2w, 0)))
  }

  if (method == "predictive") {
    return(sqrt(pmax(S2w + v_between, 0)))
  }

  # method == "mean": equicorrelated SE of weighted average of means
  if (K == 1L) return(pmax(S[, 1L], 0))             # single model: return its SE
  rho_min = -1 / (K - 1)
  if (!is.finite(rho)) stop("rho must be finite.")
  rho = min(max(rho, rho_min), 1)                  # clamp to PSD range

  Sw    = rowSums(S * rep(w, each = N))            # sum_i w_i s_i
  S2w2  = rowSums((S * S) * rep(w^2, each = N))    # sum_i w_i^2 s_i^2
  var_mean = (1 - rho) * S2w2 + rho * (Sw ^ 2)
  sqrt(pmax(var_mean, 0))
}
