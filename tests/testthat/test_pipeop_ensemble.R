context("PipeOpModelAvg")


test_that("PipeOpEnsemble - basic properties", {
  op = PipeOpEnsemble$new(3)
  expect_pipeop(op)
})


test_that("PipeOpMajorityVote - basic properties", {
  op = PipeOpMajorityVote$new(3)
  expect_pipeop(op)
})


test_that("PipeOpMajorityVote - train and predict", {

  prd = PredictionClassif$new()
  prd$row_ids = seq_len(10)
  prd$response = rep(c("a", "b"), 5)
  prd$truth = rep(c("a", "b"), 5)
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


test_that("PipeOpModelAvg - train and predict", {

  prd = PredictionRegr$new()
  prd$row_ids = seq_len(10)
  prd$response = seq_len(10)
  prd$truth = seq_len(10)
  prds = map(seq_len(3), function(x) prd$clone())

  po = PipeOpModelAvg$new(3)

  expect_list(train_pipeop(po, prds), len = 1)

  out = predict_pipeop(po, prds)
  expect_equal(out, list(prd))

  prd2 = prd$clone()
  prd2$response = 10:1

  po2 = PipeOpModelAvg$new(2)
  expect_list(train_pipeop(po2, list(prd, prd2)), len = 1)

  out2 = predict_pipeop(po2, list(prd, prd2))
  prd_ref = prd2$clone()
  prd_ref$response = rep(5.5, 10)
  expect_equivalent(out2, list(prd_ref))
})
