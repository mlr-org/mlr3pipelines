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
    train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    predict = function(inputs) stop("abstract")
  ),
  active = list(
    weights = function(val) {
      if (!missing(val)) {
        self$param_set$values$weights = val
      }
      self$param_set$values$weights
    }
  )
)

check_weights = function(innum) {
  if (innum > 0) {
    function(x) assert(
      check_numeric(x, len = innum, any.missing = FALSE),
      check_numeric(x, len = 1, any.missing = FALSE)
    )
  } else {
    function(x) assert_numeric(x, min.len = 1, any.missing = FALSE)
  }
}

#' @title PipeOpModelAvg
#'
#' @name mlr_pipeop_modelavg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`].
#'
#' @description
#' Averages its input (a `list` of `PredictionRegr`).
#' Only used for regression `Prediction`s.
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights for each prediction.
#' Offers a `$weights` slot to set/get weights for each learner.
#' Returns a single `PredictionRegr`.
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpModelAvg$new()
#' @export
PipeOpModelAvg = R6Class("PipeOpModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum = 0, id = "modelavg", param_vals = list(), ...) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionRegr", ...)
    },

    predict = function(inputs) {
      list(private$weighted_avg_predictions(inputs, self$weights))
    }),

  private = list(
    weighted_avg_predictions = function(inputs, weights) {
      row_ids = inputs[[1]]$row_ids
      map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
      truth = inputs[[1]]$truth
      if (length(weights) == 1) weights = rep(weights, length(inputs))
      weights = weights / sum(weights)
      responsematrix = simplify2array(map(inputs, "response"))
      response = c(responsematrix %*% weights)
      se = NULL
      if (all(map_lgl(inputs, function(x) "se" %in% names(x$data)))) {
        se = c(sqrt(
          (
            simplify2array(map(inputs, "se"))^2 +
            (responsematrix - response)^2
          ) %*% weights
        ))
      }
      PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se)
    }
  )
)

register_pipeop("modelavg", PipeOpModelAvg)


#' @title PipeOpMajorityVote
#'
#' @format [R6Class] PipeOpMajorityVote
#'
#' @name mlr_pipeop_majorityvote
#' @format [`R6Class`] inheriting from [`PipeOpMajorityVote`].
#'
#' @description
#' Aggregates over different `PredictionClassif`s.
#' Either computes the mode, if `predict_type` is `"response"`,
#' or averages probabilities if `predict_type` is `"prob"`.
#' Returns a single `PredictionClassif`.
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights for each prediction.
#' Used for classification `Prediction`s.
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpMajorityVote$new(3)
#' @export
PipeOpMajorityVote = R6Class("PipeOpMajorityVote",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum = 0, id = "majorityvote", param_vals = list(), ...) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionClassif", ...)
    },

    predict = function(inputs) {
      list(private$weighted_avg_predictions(inputs, self$weights))
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights) {
      row_ids = inputs[[1]]$row_ids
      map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
      truth = inputs[[1]]$truth
      if (length(weights) == 1) weights = rep(weights, length(inputs))
      weights = weights / sum(weights)

      # Drop zero-weights for efficiency
      inputs = inputs[weights != 0]
      weights = weights[weights != 0]

      has_probs = every(inputs, function(x) { !is.null(x$prob) })
      has_response = every(inputs, function(x) { !is.null(x$response) })
      if (!(has_probs || has_response)) {
        stop("PipeOpMajorityVote input predictions had missing 'prob' and missing 'response' values. At least one of them must be given for all predictions.")
      }
      prob = response = NULL
      if (has_probs) {
        prob = private$weighted_prob_avg(inputs, weights)
      }
      if (has_response) {
        response = private$weighted_majority_vote(inputs, weights)
      }
      PredictionClassif$new(row_ids = row_ids, truth = truth, response = response, prob = prob)
    },
    weighted_majority_vote = function(inputs, weights) {
      alllevels = levels(inputs[[1]]$response)
      map(inputs, function(x) assert_true(identical(sort(alllevels), sort(levels(x$response)))))

      accmat = matrix(0, nrow = length(inputs[[1]]$response), ncol = length(alllevels))
      for (idx in seq_along(inputs)) {
        rdf = data.frame(x = factor(inputs[[idx]]$response, levels = alllevels))
        curmat = model.matrix(~ 0 + x, rdf) * weights[idx]
        accmat = accmat + curmat
      }
      factor(alllevels[max.col(accmat)], levels = alllevels)
    },
    weighted_prob_avg = function(inputs, weights) {
      alllevels = colnames(inputs[[1]]$prob)
      assert_character(alllevels, any.missing = FALSE)
      map(inputs, function(x) assert_true(identical(sort(alllevels), sort(colnames(x$prob)))))

      accmat = inputs[[1]]$prob * weights[1]
      for (idx in seq_along(inputs)[-1]) {
        accmat = accmat + inputs[[idx]]$prob[, alllevels, drop = FALSE] * weights[idx]
      }
      accmat
    }
  )
)

register_pipeop("majorityvote", PipeOpMajorityVote)

