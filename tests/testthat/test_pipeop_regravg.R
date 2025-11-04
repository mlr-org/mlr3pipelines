context("PipeOpRegrAvg")

predict_regravg = function(predictions, se_aggr, weights, se_aggr_rho = NULL) {
  po = po("regravg")
  po$param_set$values$weights = weights
  po$param_set$values$se_aggr = se_aggr
  if (!is.null(se_aggr_rho)) {
    po$param_set$values$se_aggr_rho = se_aggr_rho
  }
  train_nulls = replicate(length(predictions), NULL, simplify = FALSE)
  po$train(train_nulls)
  po$predict(predictions)[[1]]
}

test_that("PipeOpRegrAvg se aggregation methods return expected SE", {
  row_ids = 1:2
  truth = c(0.5, -1.2)
  responses = list(
    c(1, 4),
    c(2, 5),
    c(6, 7)
  )
  ses = list(
    c(0.2, 0.3),
    c(0.4, 0.5),
    c(0.6, 0.7)
  )
  weights = c(0.2, 0.3, 0.5)

  make_predictions = function(responses, ses_list = NULL) {
    lapply(seq_along(responses), function(i) {
      args = list(
        row_ids = row_ids,
        truth = truth,
        response = responses[[i]]
      )
      if (!is.null(ses_list)) {
        args$se = ses_list[[i]]
      }
      do.call(PredictionRegr$new, args)
    })
  }

  preds_with_se = make_predictions(responses, ses)
  preds_without_se = make_predictions(responses, NULL)

  response_matrix = do.call(cbind, responses)
  expected_response = as.numeric(response_matrix %*% weights)

  weight_matrix = matrix(weights, nrow = length(row_ids), ncol = length(weights), byrow = TRUE)
  between = rowSums((response_matrix^2) * weight_matrix) - expected_response^2
  between = pmax(between, 0)
  expected_between = sqrt(between)

  se_matrix = do.call(cbind, ses)
  within = rowSums((se_matrix^2) * weight_matrix)
  within = pmax(within, 0)
  expected_within = sqrt(within)
  expected_predictive = sqrt(within + between)

  weight_matrix_sq = matrix(weights^2, nrow = length(row_ids), ncol = length(weights), byrow = TRUE)
  Sw = rowSums(se_matrix * weight_matrix)
  S2w2 = rowSums((se_matrix^2) * weight_matrix_sq)
  expected_mean_rho0 = sqrt(pmax(S2w2, 0))
  expected_mean_rho1 = sqrt(pmax(Sw^2, 0))
  rho_min = -1 / (length(weights) - 1)
  var_rho_min = (1 - rho_min) * S2w2 + rho_min * (Sw^2)
  expected_mean_rho_min = sqrt(pmax(var_rho_min, 0))

  pred_none = predict_regravg(preds_with_se, "none", weights)
  expect_equal(pred_none$response, expected_response)
  expect_true(all(is.na(pred_none$se)))
  expect_false("se" %in% names(pred_none$data))

  pred_between = predict_regravg(preds_without_se, "between", weights)
  expect_equal(pred_between$response, expected_response)
  expect_equal(pred_between$se, expected_between)

  pred_within = predict_regravg(preds_with_se, "within", weights)
  expect_equal(pred_within$response, expected_response)
  expect_equal(pred_within$se, expected_within)

  pred_predictive = predict_regravg(preds_with_se, "predictive", weights)
  expect_equal(pred_predictive$response, expected_response)
  expect_equal(pred_predictive$se, expected_predictive)

  pred_mean_indep = predict_regravg(preds_with_se, "mean", weights, se_aggr_rho = 0)
  expect_equal(pred_mean_indep$response, expected_response)
  expect_equal(pred_mean_indep$se, expected_mean_rho0)

  pred_mean_full = predict_regravg(preds_with_se, "mean", weights, se_aggr_rho = 1)
  expect_equal(pred_mean_full$response, expected_response)
  expect_equal(pred_mean_full$se, expected_mean_rho1)

  pred_mean_clamped = predict_regravg(preds_with_se, "mean", weights, se_aggr_rho = -1)
  expect_equal(pred_mean_clamped$response, expected_response)
  expect_equal(pred_mean_clamped$se, expected_mean_rho_min)
})

test_that("PipeOpRegrAvg se aggregation requiring SE errors when SE is missing", {
  responses = list(
    c(1, 2),
    c(3, 4)
  )
  weights = c(0.5, 0.5)
  preds_without_se = lapply(responses, function(resp) {
    PredictionRegr$new(row_ids = 1:2, truth = c(0, 0), response = resp)
  })

  expect_error(
    predict_regravg(preds_without_se, "predictive", weights),
    "requires `ses_list`"
  )

  expect_error(
    predict_regravg(preds_without_se, "mean", weights),
    "requires `ses_list`"
  )

  expect_error(
    predict_regravg(preds_without_se, "within", weights),
    "requires `ses_list`"
  )
})

test_that("PipeOpRegrAvg se aggregation with single prediction behaves correctly", {
  row_ids = 1:4
  truth = c(0, 1, 2, 3)
  response = c(1.1, 2.2, 3.3, 4.4)
  se = c(0.5, 0.6, 0.7, 0.8)
  single_pred_with_se = list(PredictionRegr$new(
    row_ids = row_ids,
    truth = truth,
    response = response,
    se = se
  ))
  single_pred_without_se = list(PredictionRegr$new(
    row_ids = row_ids,
    truth = truth,
    response = response
  ))

  result_none = predict_regravg(single_pred_with_se, "none", weights = 1)
  expect_equal(result_none$response, response)
  expect_false("se" %in% names(result_none$data))

  result_between = predict_regravg(single_pred_without_se, "between", weights = 1)
  expect_equal(result_between$response, response)
  expect_equal(result_between$se, rep(0, length(response)))

  result_within = predict_regravg(single_pred_with_se, "within", weights = 1)
  expect_equal(result_within$response, response)
  expect_equal(result_within$se, se)

  result_predictive = predict_regravg(single_pred_with_se, "predictive", weights = 1)
  expect_equal(result_predictive$response, response)
  expect_equal(result_predictive$se, se)

  result_mean = predict_regravg(single_pred_with_se, "mean", weights = 1, se_aggr_rho = 0.25)
  expect_equal(result_mean$response, response)
  expect_equal(result_mean$se, se)
})
