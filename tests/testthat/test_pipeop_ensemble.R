context("PipeOpModelAvg")

test_that("PipeOpEnsemble - basic properties", {
  op = PipeOpEnsemble$new(3, "ensemble", param_vals = list())
  expect_pipeop(op)
  expect_pipeop_class(PipeOpEnsemble, list(3, "ensemble", param_vals = list()))
  expect_pipeop_class(PipeOpEnsemble, list(1, "ensemble", param_vals = list()))
  expect_error(PipeOpEnsemble$new(0))

  truth = rnorm(70)
  prds = replicate(4, PredictionRegr$new(row_ids = seq_len(70), truth = truth, response = truth + rnorm(70, sd = 0.1)))
  expect_list(train_pipeop(op, prds), len = 1)
  expect_error(predict_pipeop(op, prds))
})

test_that("PipeOpWeightedModelAvg - train and predict", {
  # Create 4 predictions
  truth = rnorm(70)
  prds = replicate(4, PredictionRegr$new(row_ids = seq_len(70), truth = truth, response = truth + rnorm(70, sd = 0.1)), simplify = FALSE)

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

## test_that("PipeOpNlOptModelAvg - response - train and predict", {
##   truth = rnorm(70)
##   prds = replicate(7, set_class(list(row_ids = seq_len(70), response = truth + rnorm(70, sd = 0.1)),
##     c("PredictionRegr", "Prediction")), simplify = FALSE)

##   po = PipeOpNlOptModelAvg$new(7)
##   expect_pipeop(po)
##   expect_list(train_pipeop(po, prds), len = 1)
##   out = predict_pipeop(po, prds)
##   expect_class(out[[1]], "PredictionRegr")
## })

test_that("PipeOpWeightedMajorityVote - response -train and predict", {
  prds = replicate(4,
    make_prediction_obj_classif(n = 100, noise = TRUE,
      predict_types = c("response"), nclasses = 3),
    simplify = FALSE
  )
  lapply(prds, function(x) x$data$truth = prds[[1]]$data$truth)  # works because of R6 reference semantics
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
  expect_equal(out[[1]]$data, prds[[4]]$data)
})

test_that("PipeOpWeightedMajorityVote - prob - train and predict", {
  prds = replicate(4,
    make_prediction_obj_classif(n = 100, noise = TRUE,
      predict_types = c("response", "prob"), nclasses = 3),
    simplify = FALSE
  )
  lapply(prds, function(x) x$data$truth = prds[[1]]$data$truth)  # works because of R6 reference semantics
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

## test_that("PipeOpNlOptMajorityVote - response - train and predict", {
##   prds = replicate(5,
##     make_prediction_obj_classif(n = 100, noise = TRUE,
##       predict_types = c("response"), nclasses = 3)
##   )
##   po = PipeOpNlOptMajorityVote$new(5)
##   expect_pipeop(po)
##   expect_list(train_pipeop(po, prds), len = 1)
##   out = predict_pipeop(po, prds)
##   expect_class(out[[1]], "PredictionClassif")
## })


## test_that("PipeOpNlOptMajorityVote - prob - train and predict", {
##   prds = replicate(3,
##     make_prediction_obj_classif(n = 100, noise = TRUE,
##       predict_types = c("response", "prob"), nclasses = 2)
##   )
##   po = PipeOpNlOptMajorityVote$new(3)
##   expect_pipeop(po)
##   expect_list(train_pipeop(po, prds), len = 1)
##   out = predict_pipeop(po, prds)
##   expect_numeric(po$state$weights)
##   expect_class(out[[1]], "PredictionClassif")

##   po = PipeOpNlOptMajorityVote$new(3)
##   expect_pipeop(po)
##   expect_list(train_pipeop(po, prds), len = 1)
##   out = predict_pipeop(po, prds)
##   expect_numeric(po$state$weights)
##   expect_class(out[[1]], "PredictionClassif")
## })
