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

test_that("PipeOpModelAvg - basic properties", {
  op = PipeOpModelAvg$new(3)
  expect_pipeop(op)
  expect_pipeop_class(PipeOpModelAvg, list(3))
  expect_pipeop_class(PipeOpModelAvg, list(1))
  expect_error(PipeOpModelAvg$new(0))
})

test_that("PipeOpModelAvg - train and predict", {

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
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)

})



test_that("PipeOpMajorityVote - basic properties", {
  op = PipeOpMajorityVote$new(3)
  expect_pipeop(op)
  expect_pipeop_class(PipeOpMajorityVote, list(3))
  expect_pipeop_class(PipeOpMajorityVote, list(1))
  expect_error(PipeOpMajorityVote$new(0))
})


test_that("PipeOpMajorityVote - response - train and predict", {

  prd = PredictionClassif$new()
  prd$row_ids = seq_len(10)
  prd$response = factor(rep(c("a", "b"), 5))
  prd$truth = factor(rep(c("a", "b"), 5))
  prd$predict_types = "response"
  prds = map(seq_len(3), function(x) prd$clone())

  po = PipeOpMajorityVote$new(3)

  expect_list(train_pipeop(po, prds), len = 1)

  out = predict_pipeop(po, prds)
  expect_equal(out, list(prd))
})

test_that("PipeOpMajorityVote - prob - train and predict", {

  prds = replicate(4,
    make_prediction_obj_classif(n = 100, noise = FALSE,
      predict_types = c("response", "prob"), nclasses = 3)
  )
  po = PipeOpMajorityVote$new(3)
  expect_list(train_pipeop(po, prds), len = 1)

  out = predict_pipeop(po, prds)
  expect_equal(out, list(prds[[1]]))
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

  po = PipeOpWtModelAvg$new(4)
  expect_pipeop(po)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)

  # Returns the same if weights are 1, rest 0
  po = PipeOpWtModelAvg$new(4)
  po$weights = c(0, 0, 1, 0)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_equal(out, list(prds[[3]]))

})

test_that("PipeOpWeightedMajorityVote - response -train and predict", {

  prds = replicate(4,
    make_prediction_obj_classif(n = 100, noise = TRUE,
      predict_types = c("response"), nclasses = 3)
  )

  po = PipeOpWtMajorityVote$new(4)
  expect_pipeop(po)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")

  po = PipeOpWtMajorityVote$new(4)
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

  po = PipeOpWtMajorityVote$new(4)
  expect_pipeop(po)
  expect_list(train_pipeop(po, prds), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")

  po = PipeOpWtMajorityVote$new(4)
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
})
