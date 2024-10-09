context("PipeOpLearnerCVPlusPlus")

test_that("PipeOpLearnerCVPlus - basic properties", {
  lrn = mlr_learners$get("regr.featureless")
  po = PipeOpLearnerCVPlus$new(lrn)

  expect_pipeop(po$clone(), check_ps_default_values = FALSE)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  task = mlr_tasks$get("mtcars")
  result = train_pipeop(po, list(task = task))
  expect_null(result[[1L]])
  expect_list(po$state$cv_models)
  expect_data_table(po$state$residuals)

  expect_equal(po$predict_type, c("response", "quantiles"))
  prds = predict_pipeop(po, list(task = task))
  expect_class(prds, "Prediction")
  expect_true(all(c("response", "quantiles") %in% names(prds)))
  expect_true(nrow(prds$quantiles) == nrow(task))
  expect_equal(dim(predictions$quantiles), c(task$nrow, 3))  # lower and upper quantiles

  expect_error(PipeOpLearnerCVPlus$new())

  expect_pipeop(po("learner_cv_plus", lrn("regr.featureless")))
})

test_that("PipeOpLearnerCVPlus - param set and values", {
  skip_if_not_installed("ranger")
  lrn = mlr_learners$get("regr.ranger")
  po = PipeOpLearnerCVPlus$new(lrn)

  expect_subset(c("mtry", "folds", "alpha"), po$param_set$ids())
  expect_equal(polrn$param_set$values, list(cvplus.folds = 3, cvplus.alpha = 0.05, num.threads = 1))

  polrn$param_set$values$mtry = 10
  expect_equal(po$param_set$values, list(cvplus.folds = 3, cvplus.alpha = 0.05, mtry = 10, num.threads = 1))

  po$param_set$values$cvplus.folds = 5
  expect_equal(po$param_set$values, list(cvplus.folds = 5, cvplus.alpha = 0.05, mtry = 10, num.threads = 1))

  expect_error(PipeOpLearnerCVPlus$new(lrn, param_vals = list(cvplus.folds = 1)))
  expect_error(PipeOpLearnerCVPlus$new(lrn, param_vals = list(cvplus.alpha = 0)))

  lrn_classif = mlr_learners$get("classif.featureless")
  expect_error(PipeOpLearnerCVPlus$new(lrn_classif))
})

test_that("PipeOpLearnerCVPlus - graph but no id", {
  skip_if_not_installed("rpart")
  g = PipeOpNOP$new() %>>% PipeOpLearner$new(LearnerRegrRpart$new())
  po = PipeOpLearnerCVPlus$new(g)
  expect_string(po$id)
})

test_that("PipeOpLearnerCVPlus - model active binding to state", {
  lrn = mlr_learners$get("regr.featureless")
  po = PipeOpLearnerCVPlus$new(lrn)
  task = mlr_tasks$get("mtcars")

  # before training states are NULL
  expect_null(po$state)
  expect_equal(po$learner$state, po$state)
  expect_equal(po$learner_model$state, po$state)

  train_out = po$train(list(task))
  train_state = po$state

  # after predicting states are unchanged
  predict_out = po$predict(list(task))
  expect_equal(po$state, train_state)
})

test_that("PipeOpLearnerCVPlus - predict_type is fixed", {
  skip_if_not_installed("ranger")
  learner = lrn("regr.ranger")
  po = PipeOpLearnerCVPlus$new(learner)
  expect_equal(po$predict_type, c("response", "quantiles"))
})

test_that("marshal", {
  task = tsk("mtcars")
  po_lrn = as_pipeop(po("learner_cv_plus", learner = lrn("regr.debug")))
  po_lrn$train(list(task))
  po_state = po_lrn$state
  expect_class(po_state, "pipeop_learner_cv_plus_state")
  po_state_marshaled = marshal_model(po_state, inplace = FALSE)
  expect_class(po_state_marshaled, "pipeop_learner_cv_plus_state_marshaled")
  expect_true(is_marshaled_model(po_state_marshaled))
  expect_equal(po_state, unmarshal_model(po_state_marshaled))
})
