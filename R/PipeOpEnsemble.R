#' @title PipeOpEnsemble
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Parent class for PipeOps that aggregate a list of predictions.
#'
#' @section Methods:
#' * `PipeOpEnsemble$new(innum = 0, id)` \cr
#'   (`numeric(1)`, `character(1)`) -> `self` \cr
#'   Constructor. `innum` determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input
#'   channel is created that can take an arbitrary number of inputs.
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
    predict_internal = function(inputs) stop("abstract")
  )
)

check_weights = function(innum) {
  if (innum == 0) {
    function(x) assert_numeric(x, any.missing = FALSE)
  } else {
    function(x)
      assert(check_numeric(x, len = innum, any.missing = FALSE),
        check_numeric(x, len = 1, any.missing = FALSE))
  }
}

weighted_avg_predictions = function(inputs, weights) {
  row_ids = inputs[[1]]$row_ids
  map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
  truth = inputs[[1]]$truth
  if (length(weights) == 1) weights = rep(weights, length(inputs))
  weights = weights / sum(weights)

  # Drop zero-weights for efficiency
  # FIXME: this is not numerically stable
  # Note: this is the behaviour of stats:::weighted.mean.default
  inputs = inputs[weights != 0]
  weights = weights[weights != 0]

  if ("PredictionClassif" %in% class(inputs[[1]])) {
    has_probs = every(inputs, function(x) !is.null(x$prob))
    has_classif_response = every(inputs, function(x) !is.null(x$response))
    if (!(has_probs || has_response)) {
      stop("PipeOpMajorityVote input predictions had missing 'prob' and missing 'response' values. At least one of them must be given for all predictions.")
    }
    prob = response = NULL
    if (has_probs) {
      alllevels = colnames(inputs[[1]]$prob)
      assert_character(alllevels, any.missing = FALSE, len = ncol(inputs[[1]]$prob))
      matrices = map(inputs, function(x) {
        mat = x$prob
        assert_set_equal(alllevels, colnames(mat))
        mat[, alllevels, drop = FALSE]
      })
      prob = weighted_matrix_sum(matrices, weights)
    }

    if (has_classif_response) {
      alllevels = levels(inputs[[1]]$response)
      map(inputs, function(x) assert_set_equal(alllevels, levels(x$response)))
      response = weighted_factor_mean(map(inputs, "response"), weights, alllevels)
    }

    return(PredictionClassif$new(row_ids = row_ids, truth = truth, response = response, prob = prob))
  }
  if ("PredictionRegr" %in% class(inputs[[1]])) {
    has_se = every(inputs, function(x) "se" %in% names(x$data))
    est_se = if (has_se) "both" else "between"

    response_matrix = simplify2array(map(inputs, "response"))
    response = c(response_matrix %*% weights)

    se = weighted_se(response_matrix, simplify2array(map(inputs, "se")), response, est_se)

    return(PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se))
  }
  stopf("Unsupported prediction type %s", class(inputs[[1]])[1])
}

weighted_matrix_sum = function(matrices, weights) {
  accmat = matrices[[1]] * weights[1]
  for (idx in seq_along(matrices)[-1]) {
    accmat = accmat + matrices[[idx]] * weights[idx]
  }
  accmat
}

weighted_factor_mean = function(factors, weights, alllevels) {
  accmat = matrix(0, nrow = length(factors), ncol = length(alllevels))
  for (idx in seq_along(factors)) {
    rdf = data.frame(x = factor(factors[[idx]], levels = alllevels))
    curmat = stats::model.matrix(~ 0 + x, rdf) * weights[idx]
    accmat = accmat + curmat
  }
  response = factor(alllevels[max.col(accmat)], levels = alllevels)
}

weighted_se = function(response_matrix, se_matrix, response, est_se) {
  assert_choice(est_se, c("within", "between", "both"))
  if (est_se != "between") {
    within_var = se_matrix^2 %*% weights
  }
  if (est_se != "within") {
    # Weighted SE calculated as in
    # https://www.gnu.org/software/gsl/doc/html/statistics.html#weighted-samples
    between_var = (response_matrix - response)^2 %*% weights / (1 - sum(weights^2) / sum(weights)^2)
  }
  c(sqrt(switch(est_se,
    within = within_var,
    between = between_var,
    both = within_var + between_var)))
}
