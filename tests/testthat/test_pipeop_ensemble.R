context("PipeOpModelAvg")

test_that("PipeOpEnsemble - basic properties", {
  op = PipeOpEnsemble$new(3, "ensemble", param_vals = list())
  expect_pipeop(op)
  expect_pipeop_class(PipeOpEnsemble, list(3, "ensemble", param_vals = list()))
  expect_pipeop_class(PipeOpEnsemble, list(1, "ensemble", param_vals = list()))
  expect_error(PipeOpEnsemble$new(0))

  truth = rnorm(70)
  prds = replicate(4, {
    prd = PredictionRegr$new()
    prd$row_ids = seq_len(70)
    prd$response = truth + rnorm(70, sd = 0.1)
    prd$truth = truth
    prd$predict_types = "response"
    return(prd)
  })
  expect_list(train_pipeop(op, prds), len = 1)
  expect_error(predict_pipeop(op, prds))
})

test_that("PipeOpWeightedModelAvg - train and predict", {
  # Create 4 predictions
  truth = rnorm(70)
  prds = replicate(4, {
    prd = PredictionRegr$new()
    prd$row_ids = seq_len(70)
    prd$response = truth + rnorm(70, sd = 0.1)
    prd$truth = truth
    prd$predict_types = "response"
    return(prd)
  })

  po = PipeOpModelAvg$new(4)
  expect_pipeop(po)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)

  # Returns the same if weights are 1, rest 0
  po = PipeOpModelAvg$new(4)
  po$weights = c(0, 0, 1, 0)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_equal(out, list(prds[[3]]))

})

test_that("PipeOpNlOptModelAvg - response - train and predict", {
  truth = rnorm(70)
  prds = replicate(7, {
    prd = PredictionRegr$new()
    prd$row_ids = seq_len(70)
    prd$response = truth + rnorm(70, sd = 0.1)
    prd$truth = truth
    prd$predict_types = "response"
    return(prd)
  })
  po = PipeOpNlOptModelAvg$new(7)
  expect_pipeop(po)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionRegr")
})

test_that("PipeOpWeightedMajorityVote - response -train and predict", {

  prds = replicate(4,
    make_prediction_obj_classif(n = 100, noise = TRUE,
      predict_types = c("response"), nclasses = 3)
  )

  po = PipeOpMajorityVote$new(4)
  expect_pipeop(po)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")

  po = PipeOpMajorityVote$new(4)
  po$weights = c(0, 0, 0, 1)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")
  expect_equal(out[[1]], prds[[4]])

})

test_that("PipeOpWeightedMajorityVote - prob - train and predict", {

  prds = replicate(4,
    make_prediction_obj_classif(n = 100, noise = TRUE,
      predict_types = c("response", "prob"), nclasses = 3)
  )

  po = PipeOpMajorityVote$new(4)
  expect_pipeop(po)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")

  po = PipeOpMajorityVote$new(4)
  po$weights = c(0, 0, 0, 1)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")
  expect_equivalent(out[[1]], prds[[4]])

})

test_that("PipeOpNlOptMajorityVote - response - train and predict", {
  prds = replicate(5,
    make_prediction_obj_classif(n = 100, noise = TRUE,
      predict_types = c("response"), nclasses = 3)
  )
  po = PipeOpNlOptMajorityVote$new(5)
  expect_pipeop(po)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")
})

test_that("PipeOpNlOptMajorityVote - prob - train and predict", {
  prds = replicate(3,
    make_prediction_obj_classif(n = 100, noise = TRUE,
      predict_types = c("response", "prob"), nclasses = 2)
  )
  po = PipeOpNlOptMajorityVote$new(3)
  expect_pipeop(po)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_numeric(po$state$weights)
  expect_class(out[[1]], "PredictionClassif")

  po = PipeOpNlOptMajorityVote$new(3)
  expect_pipeop(po)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_numeric(po$state$weights)
  expect_class(out[[1]], "PredictionClassif")
})

test_that("PipeOps break for bad inputs", {
  clprds = replicate(3,
    make_prediction_obj_classif(n = 100, noise = TRUE,
      predict_types = c("response", "prob"), nclasses = 2)
  )

  truth = rnorm(70)
  rgrprds = replicate(7, {
    prd = PredictionRegr$new()
    prd$row_ids = seq_len(70)
    prd$response = truth + rnorm(70, sd = 0.1)
    prd$truth = truth
    prd$predict_types = "response"
    return(prd)
  })

  cpo2 = PipeOpNlOptMajorityVote$new(3)
  rpo2 = PipeOpNlOptModelAvg$new(3)

  expect_error(train_pipeop(rpo2, clprds))
  expect_error(train_pipeop(cpo2, rgrprds))
  expect_error(train_pipeop(cpo2, clprds[[1]]))
  expect_error(train_pipeop(rpo2, rgrprds[[1]]))

  rpo2$param_set$values$measure = mlr_measures$get("classif.ce")
  cpo2$param_set$values$measure = mlr_measures$get("regr.mse")
  expect_error(train_pipeop(rpo2, rgrprds))
  expect_error(train_pipeop(cpo2, clprds))
})
