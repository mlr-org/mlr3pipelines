
#' Perform (weighted) prediction averaging from regression [`Prediction`][mlr3::prediction]s by connecting
#' [`PipeOpModelAvg`] to multiple [`PipeOpLearner`] outputs.
#'
#' The resulting `"response"` prediction is a weighted average of the incoming `"response"` predictions.
#'
#' The `"se"` prediction is calculated from the (weighted) standard error of the `"response"` predictions and
#' (if the incoming [`Learner`][mlr3::Learner]'s `$predict_type`s are all `"se"`) the `"se"` predictions of all
#' incoming [`Prediction`][mlr3::Prediction]`s according to the following pseudocode:
#'
#' ```
#' # response: vector of N "response" values predicted by different Learners for a
#' #   prediction sample
#' # se: vector of N "se" values predicted by different Learners for a prediction
#' #   sample
#' # weights: N weights set by $param_set$values$weights
#' norm_weights = weights / sum(weights)
#'
#' var_prediction = sum( (response - sum(response * norm_weights))^2 * norm_weights^2 ) /
#'   (1 - sum(norm_weights^2))
#'
#' if (previous learners have $predict_type "se") {
#'   var_prediction = var_prediction + sum( se^2 * norm_weights^2 )
#' }
#' se_prediction = sqrt(var_prediction)
#' ```
#'
PipeOpModelAvg = R6Class("PipeOpModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum = 0, id = "modelavg", param_vals = list(), ...) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionRegr", ...)
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights, row_ids, truth) {
      has_se = every(inputs, function(x) "se" %in% names(x$data))
      est_se = if (has_se) "both" else "between"

      response_matrix = simplify2array(map(inputs, "response"))
      response = c(response_matrix %*% weights)
      if (has_se || length(inputs) > 1) {
        if (length(inputs) == 1) {
          est_se = "within"
        }
        se = weighted_se(response_matrix, simplify2array(map(inputs, "se")), response, weights, est_se)
      } else {
        se = NULL
      }

      PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se)
    }
  )
)

mlr_pipeops$add("modelavg", PipeOpModelAvg)



#' * `est_se` :: `character(1)` \cr
#'   How to estimate standard error, if `predict_type` is `"se"`. Can be `"within"` (calculate SE from
#'   input SEs; input data must contain `"se"`-prediction), `"between"` (calculate SE from SE of
#'   predictions, does not need `"se"`-prediction of previous Learners), and `"both"` (square root of the sum
#'   of the squared `"within"` and `"between"` se estimates).
LearnerRegrWeightedAverage = R6Class("LearnerRegrWeightedAverage", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.weightedavg") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          params = list(
            ParamUty$new(id = "measure", tags = c("train", "required")),
            ParamFct$new(id = "algorithm", tags = c("train", "required"), levels = nlopt_levels),
            ParamFct$new(id = "est_se", tags = c("train", "predict", "required"), levels = c("within", "between", "both"))
          )
        ),
        param_vals = list(measure = "regr.mse", algorithm = "NLOPT_LN_COBYLA", est_se = "both"),
        predict_types = c("response", "se"),
        feature_types = c("integer", "numeric")
      )
    },

    train_internal = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = self$prepare_data(task)
      n_weights = ncol(data$response_matrix)
      list("weights" = optimize_objfun_nlopt(task, pars, self$weighted_average_prediction, n_weights, data))
    },

    predict_internal = function(task) {
      self$weighted_average_prediction(task, self$model$weights, self$prepare_data(task))
    },
    prepare_data = function(task) {
      response_matrix = as.matrix(task$data(cols = grep("\\.response$", task$feature_names, value = TRUE)))
      est_se = self$param_set$values$est_se
      se_matrix = NULL
      if (self$predict_type == "se" && est_se != "between") {
        se_matrix = as.matrix(task$data(cols = grep("\\.se$", task$feature_names, value = TRUE)))
        if (ncol(se_matrix) != ncol(response_matrix)) {
          stopf("est_se is '%s', but not all incoming Learners provided 'se'-prediction", est_se)
        }
      }
      list(
        response_matrix = response_matrix,
        se_matrix = se_matrix
      )
    },
    weighted_average_prediction = function(task, weights, data) {
      wts = weights / sum(weights)

      response = c(data$response_matrix %*% wts)
      se = NULL
      if (self$predict_type == "se") {
        se = weighted_se(data$response_matrix, data$se_matrix, response, weights, self$param_set$values$est_se)
      }
      PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), response = response, se = se)
    }
  )
)



# Weighted standard error. Depending on `est_se`, this is either
# ("within") the square root mean square  of `se_matrix`,
# ("between") the standard error of responses in `response_matrix`,
# or ("both") the square root of the sum of the squares of both.
# @param response_matrix [`matrix`] matrix of response values; not used for `est_se` == "within"
# @param se_matrix [`matrix`] matrix of `se` values; not used for `est_se` == "between"
# @param response [`numeric`] (weighted) mean of response values
# @param weights [`numeric`] weights
# @param est_se [`character(1)`] "within", "between", or "both"
# TODO: find a justification for this
weighted_se = function(response_matrix, se_matrix, response, weights, est_se) {
  assert_choice(est_se, c("within", "between", "both"))
  if (est_se != "between") {
    within_var = se_matrix^2 %*% weights^2
  }
  if (est_se != "within") {
    # Weighted SE calculated as in
    # https://www.gnu.org/software/gsl/doc/html/statistics.html#weighted-samples
    between_var = (response_matrix - response)^2 %*% weights^2 / (1 - sum(weights^2))
    between_var[is.nan(between_var)] = 0
  }
  c(sqrt(switch(est_se,
    within = within_var,
    between = between_var,
    both = within_var + between_var)))
}
