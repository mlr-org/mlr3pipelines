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
      map(inputs, function(x) assert_true(identical(sort(alllevels), sort(colnames(x$prob)))))

      accmat = inputs[[1]]$prob * weights[1]
      for (idx in seq_along(inputs)[-1]) {
        accmat = accmat + inputs[[idx]]$prob[, alllevels, drop = FALSE] * weights[idx]
      }
      prob = accmat
    }

    if (has_classif_response) {
      alllevels = levels(inputs[[1]]$response)
      map(inputs, function(x) assert_true(identical(sort(alllevels), sort(levels(x$response)))))

      accmat = matrix(0, nrow = length(inputs[[1]]$response), ncol = length(alllevels))
      for (idx in seq_along(inputs)) {
        rdf = data.frame(x = factor(inputs[[idx]]$response, levels = alllevels))
        curmat = stats::model.matrix(~ 0 + x, rdf) * weights[idx]
        accmat = accmat + curmat
      }
      response = factor(alllevels[max.col(accmat)], levels = alllevels)
    }

    return(PredictionClassif$new(row_ids = row_ids, truth = truth, response = response, prob = prob))
  }
  if ("PredictionRegr" %in% class(inputs[[1]])) {
    has_se = every(inputs, function(x) "se" %in% names(x$data))

    responsematrix = simplify2array(map(inputs, "response"))
    response = c(responsematrix %*% weights)
    se = NULL
    if (has_se) {
      # Weighted SE as done in
      # https://www.gnu.org/software/gsl/doc/html/statistics.html#weighted-samples
      se = c(sqrt(
        (
          simplify2array(map(inputs, "se"))^2 +
          (responsematrix - response)^2
        ) %*% weights /
        (1 - sum(weights^2) / sum(weights)^2)
      ))
    }
    return(PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se))
  }
  stopf("Unsupported prediction type %s", class(inputs[[1]])[1])
}
