context("PipeOpLearnerPICVPlus")

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


test_that("PipeOpLearnerPICVPlus - basic properties", {
  lrn = mlr_learners$get("regr.featureless")
  po = PipeOpLearnerPICVPlus$new(lrn)

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

  expect_error(PipeOpLearnerPICVPlus$new(), "learner")

  expect_pipeop(po("learner_pi_cvplus", lrn("regr.featureless")))
})

test_that("PipeOpLearnerPICVPlus - param set and values", {
  skip_if_not_installed("rpart")
  lrn = mlr_learners$get("regr.rpart")
  po = PipeOpLearnerPICVPlus$new(lrn)

  expect_subset(c("minsplit", "picvplus.folds", "picvplus.alpha"), po$param_set$ids())
  expect_equal(po$param_set$values, list(picvplus.folds = 3, picvplus.alpha = 0.05, xval = 0))

  po$param_set$values$minsplit = 2
  expect_equal(po$param_set$values, list(picvplus.folds = 3, picvplus.alpha = 0.05, minsplit = 2, xval = 0))

  po$param_set$values$picvplus.folds = 5
  expect_equal(po$param_set$values, list(picvplus.folds = 5, picvplus.alpha = 0.05, minsplit = 2, xval = 0))

  expect_error(PipeOpLearnerPICVPlus$new(lrn, param_vals = list(picvplus.folds = 1)), "is not >= 1")
  expect_error(PipeOpLearnerPICVPlus$new(lrn, param_vals = list(picvplus.alpha = -1)), "is not >= -")

  lrn_classif = mlr_learners$get("classif.featureless")
  expect_error(PipeOpLearnerPICVPlus$new(lrn_classif), "only supports regression")
})

test_that("PipeOpLearnerPICVPlus - graph but no id", {
  skip_if_not_installed("rpart")
  g = PipeOpNOP$new() %>>% PipeOpLearner$new(LearnerRegrRpart$new())
  po = PipeOpLearnerPICVPlus$new(g)
  expect_string(po$id)
})

test_that("PipeOpLearnerPICVPlus - model active binding to state", {
  lrn = mlr_learners$get("regr.featureless")
  po = PipeOpLearnerPICVPlus$new(lrn)
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

test_that("PipeOpLearnerPICVPlus - predict_type is fixed", {
  skip_if_not_installed("rpart")
  lrn = lrn("regr.rpart")
  po = PipeOpLearnerPICVPlus$new(lrn)
  expect_equal(po$predict_type, c("response", "quantiles"))
})

test_that("PipeOpLearnerPICVPlus - integration with larger graph", {
  skip_if_not_installed("rpart")

  task = mlr_tasks$get("mtcars")
  lrn = mlr_learners$get("regr.rpart")

  po_picvplus = PipeOpLearnerPICVPlus$new(lrn)
  po_nop = PipeOpNOP$new()
  graph = po_picvplus %>>% po_nop

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
  po = PipeOpLearnerPICVPlus$new(lrn)
  po$train(list(task))
  po_state = po$state

  expect_class(po_state, "pipeop_learner_pi_cvplus_state")

  po_state_marshaled = marshal_model(po_state, inplace = FALSE)

  expect_class(po_state_marshaled, "pipeop_learner_pi_cvplus_state_marshaled")
  expect_true(is_marshaled_model(po_state_marshaled))

  po_state_unmarshaled = unmarshal_model(po_state_marshaled)
  expect_equal(po_state, po_state_unmarshaled)
})

test_that("marshal multiplicity", {
  lrn = lrn("regr.debug")
  lrn$properties = c(lrn$properties, "marshal")
  po = PipeOpLearnerPICVPlus$new(lrn)

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
  po = PipeOpLearnerPICVPlus$new(lrn)

  task1 = mlr_tasks$get("mtcars")
  task2 = mlr_tasks$get("boston_housing")
  input = Multiplicity(task1, task2)

  po$train(list(input))
  expect_class(po$state, "Multiplicity")
  expect_class(po$state[[1L]], "pipeop_learner_pi_cvplus_state")
  expect_class(po$learner_model, "Multiplicity")
  expect_class(po$learner_model[[1L]][[1L]], "LearnerRegr")
  expect_equal(length(po$learner_model), length(input))
  expect_equal(length(po$learner_model[[1]]), po$param_set$values$picvplus.folds)

  prds = po$predict(list(input))
  expect_class(prds$output, "Multiplicity")
  expect_equal(length(prds$output), length(input))
  expect_class(prds$output[[1L]], "PredictionRegr")

  # recursive
  po1 = PipeOpLearnerPICVPlus$new(lrn)
  po1$train(list(Multiplicity(input)))
  expect_class(po1$state, "Multiplicity")
  expect_class(po1$state[[1L]], "Multiplicity")
  expect_class(po1$state[[1L]][[1L]], "pipeop_learner_pi_cvplus_state")

  expect_class(po1$learner_model, "Multiplicity")
  expect_class(po1$learner_model[[1L]], "Multiplicity")
  expect_class(po1$learner_model[[1L]][[1L]][[1]], "LearnerRegr")
  expect_equal(length(po1$learner_model[[1L]]), length(input))
  expect_equal(length(po1$learner_model[[1L]][[1L]]), po1$param_set$values$picvplus.folds)

  prds1 = po1$predict(list(Multiplicity(input)))
  expect_class(prds1$output, "Multiplicity")
  expect_class(prds1$output[[1L]], "Multiplicity")
  expect_class(prds1$output[[1L]][[1L]], "PredictionRegr")
  expect_equal(length(prds1$output[[1L]]), length(input))
})
