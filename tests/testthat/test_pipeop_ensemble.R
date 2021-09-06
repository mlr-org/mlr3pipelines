context("PipeOpEnsemble")

test_that("PipeOpEnsemble - basic properties", {
  op = PipeOpEnsemble$new(4, id = "ensemble", param_vals = list())
  expect_pipeop(op)
  expect_pipeop_class(PipeOpEnsemble, list(3, id = "ensemble", param_vals = list()))
  expect_pipeop_class(PipeOpEnsemble, list(1, id = "ensemble", param_vals = list()))
  expect_pipeop_class(PipeOpEnsemble, list(0, id = "ensemble", param_vals = list()))

  truth = rnorm(70)
  prds = replicate(4, PredictionRegr$new(row_ids = seq_len(70), truth = truth, response = truth + rnorm(70, sd = 0.1)))
  expect_list(train_pipeop(op, rep(list(NULL), 4)), len = 1)
  expect_error(predict_pipeop(op, prds), "Abstract")

  op = PipeOpEnsemble$new(0, id = "ensemble", param_vals = list())
  expect_pipeop(op)

  # collect_multiplicity = TRUE
  op = PipeOpEnsemble$new(0, collect_multiplicity = TRUE, id = "ensemble", param_vals = list())
  expect_pipeop(op)
  expect_list(train_pipeop(op, list(as.Multiplicity(rep(list(NULL), 4)))), len = 1)
  expect_error(predict_pipeop(op, list(as.Multiplicity(prds))), "Abstract")

  expect_error(PipeOpEnsemble$new(1, collect_multiplicity = TRUE, id = "ensemble", param_vals = list()), regexp = "collect_multiplicity only works with innum == 0")
})

test_that("PipeOpWeightedRegrAvg - train and predict", {
  # Create 4 predictions
  truth = rnorm(70)
  prds = replicate(4, PredictionRegr$new(row_ids = seq_len(70), truth = truth, response = truth + rnorm(70, sd = 0.1)), simplify = FALSE)

  po = PipeOpRegrAvg$new(4)
  expect_pipeop(po)
  expect_list(train_pipeop(po, rep(list(NULL), 4)), len = 1)
  out = predict_pipeop(po, prds)

  # Returns the same if weights are 1, rest 0
  po = PipeOpRegrAvg$new(4)
  po$param_set$values$weights = c(0, 0, 1, 0)
  expect_list(train_pipeop(po, rep(list(NULL), 4)), len = 1)
  out = predict_pipeop(po, prds)
  expect_equal(out, list(output = prds[[3]]))


  po = PipeOpRegrAvg$new()
  expect_pipeop(po)
  expect_list(train_pipeop(po, rep(list(NULL), 4)), len = 1)
  out = predict_pipeop(po, prds)

  # Returns the same if weights are 1, rest 0
  po = PipeOpRegrAvg$new()
  po$param_set$values$weights = c(0, 0, 1, 0)
  expect_list(train_pipeop(po, rep(list(NULL), 4)), len = 1)
  out = predict_pipeop(po, prds)
  expect_equal(out, list(output = prds[[3]]))
})

## test_that("PipeOpNlOptRegrAvg - response - train and predict", {
##   truth = rnorm(70)
##   prds = replicate(7, set_class(list(row_ids = seq_len(70), response = truth + rnorm(70, sd = 0.1)),
##     c("PredictionRegr", "Prediction")), simplify = FALSE)

##   po = PipeOpNlOptRegrAvg$new(7)
##   expect_pipeop(po)
##   expect_list(train_pipeop(po, prds), len = 1)
##   out = predict_pipeop(po, prds)
##   expect_class(out[[1]], "PredictionRegr")
## })

test_that("PipeOpWeightedClassifAvg - response - train and predict", {
  nulls = rep(list(NULL), 4)
  prds = replicate(4,
    make_prediction_obj_classif(n = 100, noise = TRUE,
      predict_types = "response", nclasses = 3),
    simplify = FALSE
  )
  lapply(prds, function(x) x$data$tab$truth = prds[[1]]$data$tab$truth) # works because of R6 reference semantics
  po = PipeOpClassifAvg$new(4)
  expect_pipeop(po)
  expect_list(train_pipeop(po, nulls), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")

  po = PipeOpClassifAvg$new(4)
  po$param_set$values$weights = c(0, 0, 0, 1)
  expect_list(train_pipeop(po, nulls), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")
  expect_equal(out[[1]]$data$tab, prds[[4]]$data$tab)

  po = PipeOpClassifAvg$new()
  expect_pipeop(po)
  expect_list(train_pipeop(po, nulls), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")

  po = PipeOpClassifAvg$new()
  po$param_set$values$weights = c(0, 0, 0, 1)
  expect_list(train_pipeop(po, nulls), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")
  expect_equal(out[[1]]$data$tab, prds[[4]]$data$tab)
})

test_that("PipeOpWeightedClassifAvg - prob - train and predict", {
  nulls = rep(list(NULL), 4)
  prds = replicate(4,
    make_prediction_obj_classif(n = 100, noise = TRUE,
      predict_types = c("response", "prob"), nclasses = 3),
    simplify = FALSE
  )
  lapply(prds, function(x) x$data$truth = prds[[1]]$data$truth) # works because of R6 reference semantics
  po = PipeOpClassifAvg$new(4)
  expect_pipeop(po)
  expect_list(train_pipeop(po, nulls), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")

  po = PipeOpClassifAvg$new(4)
  po$param_set$values$weights = c(0, 0, 0, 1)
  expect_list(train_pipeop(po, nulls), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")
  expect_equivalent(as.data.table(out[[1]]), as.data.table(prds[[4]]))

  po = PipeOpClassifAvg$new()
  expect_pipeop(po)
  expect_list(train_pipeop(po, nulls), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")

  po = PipeOpClassifAvg$new()
  po$param_set$values$weights = c(0, 0, 0, 1)
  expect_list(train_pipeop(po, nulls), len = 1)
  out = predict_pipeop(po, prds)
  expect_class(out[[1]], "PredictionClassif")
  expect_equivalent(as.data.table(out[[1]]), as.data.table(prds[[4]]))
})

## test_that("PipeOpNlOptClassifAvg - response - train and predict", {
##   prds = replicate(5,
##     make_prediction_obj_classif(n = 100, noise = TRUE,
##       predict_types = c("response"), nclasses = 3)
##   )
##   po = PipeOpNlOptClassifAvg$new(5)
##   expect_pipeop(po)
##   expect_list(train_pipeop(po, prds), len = 1)
##   out = predict_pipeop(po, prds)
##   expect_class(out[[1]], "PredictionClassif")
## })


## test_that("PipeOpNlOptClassifAvg - prob - train and predict", {
##   prds = replicate(3,
##     make_prediction_obj_classif(n = 100, noise = TRUE,
##       predict_types = c("response", "prob"), nclasses = 2)
##   )
##   po = PipeOpNlOptClassifAvg$new(3)
##   expect_pipeop(po)
##   expect_list(train_pipeop(po, prds), len = 1)
##   out = predict_pipeop(po, prds)
##   expect_numeric(po$state$weights)
##   expect_class(out[[1]], "PredictionClassif")

##   po = PipeOpNlOptClassifAvg$new(3)
##   expect_pipeop(po)
##   expect_list(train_pipeop(po, prds), len = 1)
##   out = predict_pipeop(po, prds)
##   expect_numeric(po$state$weights)
##   expect_class(out[[1]], "PredictionClassif")
## })
