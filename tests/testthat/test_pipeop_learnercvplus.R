context("PipeOpLearnerCVPlusPlus")

# marshaling for regr.debug
marshal_model.regr.debug_model = function(model, inplace = FALSE, ...) {
  if (!is.null(model$marshal_count)) {
    model$marshal_count = model$marshal_count + 1
  }
  structure(list(
    marshaled = model, packages = "mlr3"),
    class = c("regr.debug_model_marshaled", "marshaled")
  )
}

unmarshal_model.regr.debug_model_marshaled = function(model, inplace = FALSE, ...) {
  unmarshaled = model$marshaled
  if (!is.null(unmarshaled$marshal_pid)) {
    unmarshaled$marshal_pid = Sys.getpid()
  }
  unmarshaled
}

registerS3method("marshal_model", "regr.debug_model", marshal_model.regr.debug_model)
registerS3method("unmarshal_model", "regr.debug_model_marshaled", unmarshal_model.regr.debug_model_marshaled)


test_that("PipeOpLearnerCVPlus - basic properties", {
  lrn = mlr_learners$get("regr.featureless")
  po = PipeOpLearnerCVPlus$new(lrn)

  expect_pipeop(po$clone(), check_ps_default_values = FALSE)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  task = mlr_tasks$get("mtcars")
  result = train_pipeop(po, list(task))
  expect_null(result[[1L]])
  expect_list(po$state$cv_model_states)
  expect_data_table(po$state$residuals)

  expect_equal(po$predict_type, c("response", "quantiles"))
  prds = predict_pipeop(po, list(task))
  expect_class(prds$output, "PredictionRegr")
  expect_true(all(c("response", "quantiles") %in% names(prds$output)))
  expect_true(nrow(prds$output$quantiles) == task$nrow)
  expect_equal(dim(prds$output$quantiles), c(task$nrow, 2))  # lower and upper quantiles

  expect_error(PipeOpLearnerCVPlus$new())

  expect_pipeop(po("learner_cv_plus", lrn("regr.featureless")))
})

test_that("PipeOpLearnerCVPlus - param set and values", { ####
  skip_if_not_installed("rpart")
  lrn = mlr_learners$get("regr.rpart")
  po = PipeOpLearnerCVPlus$new(lrn)

  expect_subset(c("minsplit", "cvplus.folds", "cvplus.alpha"), po$param_set$ids())
  expect_equal(po$param_set$values, list(cvplus.folds = 3, cvplus.alpha = 0.05, xval = 0))

  po$param_set$values$minsplit = 2
  expect_equal(po$param_set$values, list(cvplus.folds = 3, cvplus.alpha = 0.05, minsplit = 2, xval = 0))

  po$param_set$values$cvplus.folds = 5
  expect_equal(po$param_set$values, list(cvplus.folds = 5, cvplus.alpha = 0.05, minsplit = 2, xval = 0))

  expect_error(PipeOpLearnerCVPlus$new(lrn, param_vals = list(cvplus.folds = 1)))
  expect_error(PipeOpLearnerCVPlus$new(lrn, param_vals = list(cvplus.alpha = -1)))

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
  expect_equal(po$learner, po$learner_model)
  expect_equal(po$learner$state, po$state)
  expect_equal(po$learner_model$state, po$state)

  train_out = po$train(list(task))
  train_state = po$state

  # after predicting states are unchanged
  predict_out = po$predict(list(task))
  expect_equal(po$state, train_state)
})

test_that("PipeOpLearnerCVPlus - predict_type is fixed", {
  skip_if_not_installed("rpart")
  lrn = lrn("regr.rpart")
  po = PipeOpLearnerCVPlus$new(lrn)
  expect_equal(po$predict_type, c("response", "quantiles"))
})

test_that("PipeOpLearnerCVPlus - integration with larger graph", {
  skip_if_not_installed("rpart")

  task = mlr_tasks$get("mtcars")
  lrn = mlr_learners$get("regr.rpart")

  po_cvplus = PipeOpLearnerCVPlus$new(lrn)
  po_nop = PipeOpNOP$new()
  graph = po_cvplus %>>% po_nop

  graph$train(task)
  predictions = graph$predict(task)[[1]]

  expect_matrix(predictions$quantiles)
  expect_true("response" %in% names(predictions))
  expect_true(inherits(predictions, "PredictionRegr"))
})

test_that("marshal", {
  lrn = lrn("regr.debug")
  lrn$properties = c(lrn$properties, "marshal")

  task = tsk("mtcars")
  po = PipeOpLearnerCVPlus$new(lrn)
  po$train(list(task))
  po_state = po$state

  expect_class(po_state, "pipeop_learner_cv_plus_state")

  po_state_marshaled = marshal_model(po_state, inplace = FALSE)

  expect_class(po_state_marshaled, "pipeop_learner_cv_plus_state_marshaled")
  expect_true(is_marshaled_model(po_state_marshaled))

  po_state_unmarshaled = unmarshal_model(po_state_marshaled)
  expect_equal(po_state, po_state_unmarshaled)
})

test_that("marshal multiplicity", {
  lrn = lrn("regr.debug")
  lrn$properties = c(lrn$properties, "marshal")
  po = PipeOpLearnerCVPlus$new(lrn)

  task1 = mlr_tasks$get("mtcars")
  task2 = mlr_tasks$get("boston_housing")

  po$train(list(Multiplicity(task1, task2)))
  state = po$state

  marshaled_state = marshal_model(state)
  expect_class(po$state, "Multiplicity")

  unmarshaled_state = unmarshal_model(marshaled_state)
  expect_equal(state, unmarshaled_state)
})

test_that("state class and multiplicity", {
  lrn = lrn("regr.debug")
  lrn$properties = c(lrn$properties, "marshal")
  po = PipeOpLearnerCVPlus$new(lrn)
  po$train(list(Multiplicity(tsk("mtcars"))))
  expect_class(po$state, "Multiplicity")
  expect_class(po$state[[1L]], "pipeop_learner_cv_plus_state")

  # recursive
  po1 = po("learner_cv_plus", learner = lrn("regr.debug"))
  po1$train(list(Multiplicity(Multiplicity(tsk("mtcars")))))
  expect_class(po1$state, "Multiplicity")
  expect_class(po1$state[[1L]], "Multiplicity")
  expect_class(po1$state[[1L]][[1L]], "pipeop_learner_cv_plus_state")
})
