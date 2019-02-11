context("PipeOpModelAvg")


test_that("PipeOpEnsemble - basic properties", {
  op = PipeOpEnsemble$new(3, "ensemble", param_vals = list())
  expect_pipeop(op)
  expect_pipeop_class(PipeOpEnsemble, list(3, "ensemble", param_vals = list()))
  expect_pipeop_class(PipeOpEnsemble, list(1, "ensemble", param_vals = list()))
  expect_error(PipeOpEnsemble$new(0))
})


test_that("PipeOpModelAvg - basic properties", {
  op = PipeOpModelAvg$new(3)
  expect_pipeop(op)
  expect_pipeop_class(PipeOpModelAvg, list(3))
  expect_pipeop_class(PipeOpModelAvg, list(1))
  expect_error(PipeOpModelAvg$new(0))
})


test_that("PipeOpMajorityVote - basic properties", {
  op = PipeOpMajorityVote$new(3)
  expect_pipeop(op)
  expect_pipeop_class(PipeOpMajorityVote, list(3))
  expect_pipeop_class(PipeOpMajorityVote, list(1))
  expect_error(PipeOpMajorityVote$new(0))
})


test_that("PipeOpMajorityVote - train and predict", {

  prd = PredictionClassif$new()
  prd$row_ids = seq_len(10)
  prd$response = rep(c("a", "b"), 5)
  prd$truth = rep(c("a", "b"), 5)
  prd$predict_types = "response"
  prds = map(seq_len(3), function(x) prd$clone())

  po = PipeOpMajorityVote$new(3)

  expect_list(train_pipeop(po, prds), len = 1)

  out = predict_pipeop(po, prds)
  expect_equal(out, list(prd))
})

test_that("PipeOpModelAvg - basic properties", {
  op = PipeOpModelAvg$new(3)
  expect_pipeop(op)
})


test_that("PipeOpWeightedModelAvg - train and predict", {

  # Create 4 predictions
  truth = rnorm(70)
  prds = replicate(4, {
    prd = PredictionRegr$new()
    prd$row_ids = seq_len(70)
    prd$response = truth + rnorm(70, sd = 0.1)
    prd$truth = truth
    return(prd)
  })

  po = PipeOpWtModelAvg$new(4)
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

  trx = sample(1:150, 80)
  prds = replicate(4, {
    lrn = mlr_learners$get("classif.rpart", predict_type = "response")
    lrn$param_set$values$maxdepth = sample(1:10, 1)
    lrn$param_set$values$minsplit = sample(1:20, 1)
    lrn$param_set$values$cp = sample(c(0.01, 0.1, 0.5), 1)
    tsk = mlr_tasks$get("iris")
    e = Experiment$new(tsk, lrn)
    e$train(trx)
    e$predict(setdiff(1:150, trx))
    e$prediction
  })

  po = PipeOpWtMajorityVote$new(4)
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

  trx = sample(1:150, 80)
  prds = replicate(4, {
    lrn = mlr_learners$get("classif.rpart", predict_type = "prob")
    lrn$param_set$values$maxdepth = sample(1:10, 1)
    lrn$param_set$values$minsplit = sample(1:20, 1)
    lrn$param_set$values$cp = sample(c(0.01, 0.1, 0.5), 1)
    tsk = mlr_tasks$get("iris")
    e = Experiment$new(tsk, lrn)
    e$train(trx)
    e$predict(setdiff(1:150, trx))
    e$prediction
  })

  po = PipeOpWtMajorityVote$new(4)
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










