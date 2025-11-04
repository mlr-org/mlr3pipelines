context("PipeOpClassifAvg")

predict_classifavg = function(predictions, prob_aggr = "mean", weights = 1, prob_aggr_eps = 1e-12) {
  po = po("classifavg")
  po$param_set$values$weights = weights
  po$param_set$values$prob_aggr = prob_aggr
  if (identical(prob_aggr, "log")) {
    po$param_set$values$prob_aggr_eps = prob_aggr_eps
  }
  train_nulls = replicate(length(predictions), NULL, simplify = FALSE)
  po$train(train_nulls)
  po$predict(predictions)[[1]]
}

test_that("PipeOpClassifAvg probability aggregation methods return expected probabilities", {
  row_ids = 1:3
  lvls = c("c0", "c1", "c2")
  truth = factor(c("c0", "c1", "c2"), levels = lvls)

  prob_mats = list(
    matrix(c(
      0.7, 0.2, 0.1,
      0.3, 0.4, 0.3,
      0.2, 0.3, 0.5
    ), ncol = length(lvls), byrow = TRUE, dimnames = list(NULL, lvls)),
    matrix(c(
      0.6, 0.1, 0.3,
      0.4, 0.3, 0.3,
      0.25, 0.25, 0.5
    ), ncol = length(lvls), byrow = TRUE, dimnames = list(NULL, lvls)),
    matrix(c(
      0.5, 0.3, 0.2,
      0.2, 0.5, 0.3,
      0.3, 0.2, 0.5
    ), ncol = length(lvls), byrow = TRUE, dimnames = list(NULL, lvls))
  )

  predictions = lapply(prob_mats, function(prob) {
    PredictionClassif$new(row_ids = row_ids, truth = truth, prob = prob)
  })
  weights = c(0.2, 0.3, 0.5)

  pred_mean = predict_classifavg(predictions, prob_aggr = "mean", weights = weights)
  expected_mean = Reduce(`+`, Map(function(prob, w) prob * w, prob_mats, weights))
  expect_equal(pred_mean$prob, expected_mean, tolerance = 1e-8)
  expected_response_mean = factor(colnames(expected_mean)[max.col(expected_mean, ties.method = "first")], levels = lvls)
  expect_equal(pred_mean$response, expected_response_mean)

  pred_log = predict_classifavg(predictions, prob_aggr = "log", weights = weights)
  expected_log = mlr3pipelines:::weighted_matrix_logpool(prob_mats, weights, epsilon = 1e-12)
  expect_equal(pred_log$prob, expected_log, tolerance = 1e-8)
  expected_response_log = factor(colnames(expected_log)[max.col(expected_log, ties.method = "first")], levels = lvls)
  expect_equal(pred_log$response, expected_response_log)
})

test_that("PipeOpClassifAvg single prediction returns input probabilities for mean and log", {
  row_ids = 1:4
  lvls = c("yes", "no")
  truth = factor(c("yes", "no", "yes", "no"), levels = lvls)
  single_prob = matrix(c(
    0.8, 0.2,
    0.1, 0.9,
    0.6, 0.4,
    0.3, 0.7
  ), ncol = length(lvls), byrow = TRUE, dimnames = list(NULL, lvls))

  prediction = list(PredictionClassif$new(row_ids = row_ids, truth = truth, prob = single_prob))

  result_mean = predict_classifavg(prediction, prob_aggr = "mean", weights = 1)
  expect_equal(result_mean$prob, single_prob, tolerance = 1e-10)

  result_log = predict_classifavg(prediction, prob_aggr = "log", weights = 1)
  expect_equal(result_log$prob, single_prob, tolerance = 1e-10)
})

test_that("PipeOpClassifAvg aggregates factor responses when probabilities are missing", {
  row_ids = 1:5
  lvls = c("a", "b")
  truth = factor(rep("a", length(row_ids)), levels = lvls)
  responses = list(
    factor(c("a", "a", "b", "a", "b"), levels = lvls),
    factor(c("b", "a", "b", "b", "b"), levels = lvls),
    factor(c("a", "b", "a", "a", "b"), levels = lvls)
  )
  predictions = lapply(responses, function(resp) {
    PredictionClassif$new(row_ids = row_ids, truth = truth, response = resp)
  })
  weights = c(0.5, 0.3, 0.2)

  result = predict_classifavg(predictions, prob_aggr = "log", weights = weights)
  expected_freq = mlr3pipelines:::weighted_factor_mean(responses, weights, lvls)
  expect_equal(result$prob, expected_freq)
  expected_response = factor(lvls[max.col(expected_freq, ties.method = "first")], levels = lvls)
  expect_equal(result$response, expected_response)
})
