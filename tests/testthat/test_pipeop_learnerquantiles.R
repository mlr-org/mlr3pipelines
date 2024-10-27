context("PipeOpLearnerQuantiles")

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


test_that("PipeOpLearnerQuantiles - basic properties", {
  lrn = mlr_learners$get("regr.featureless")
  po = PipeOpLearnerQuantiles$new(lrn)

  expect_pipeop(po$clone(), check_ps_default_values = FALSE)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  task = mlr_tasks$get("mtcars")
  result = train_pipeop(po, list(task))
  expect_null(result[[1L]])
  expect_list(po$state$model_states)
  expect_equal(length(po$state$model_states), length(po$param_set$values$quantiles.q_vals))

  expect_equal(po$predict_type, c("response", "quantiles"))
  prds = predict_pipeop(po, list(task))
  expect_class(prds$output, "PredictionRegr")
  expect_true(all(c("response", "quantiles") %in% names(prds$output)))
  expect_true(nrow(prds$output$quantiles) == task$nrow)
  expect_equal(dim(prds$output$quantiles), c(task$nrow, length(po$param_set$values$quantiles.q_vals)))

  expect_equal(prds$output$response, prds$output$quantiles[, "q0.5"])
  expect_error(PipeOpLearnerQuantiles$new(), "learner")

  expect_pipeop(po("learner_quantiles", lrn("regr.featureless")))
})

test_that("PipeOpLearnerQuantiles - param set and values", {
  lrn = mlr_learners$get("regr.featureless")
  po = PipeOpLearnerQuantiles$new(lrn)

  expect_subset(c("robust", "quantiles.q_vals", "quantiles.q_response"), po$param_set$ids())
  expect_equal(po$param_set$values, list(quantiles.q_vals = c(0.05, 0.5, 0.95), quantiles.q_response = 0.5, robust = FALSE))

  po$param_set$values$robust = TRUE
  expect_equal(po$param_set$values, list(quantiles.q_vals = c(0.05, 0.5, 0.95), quantiles.q_response = 0.5, robust = TRUE))

  po$param_set$values$quantiles.q_response = 0.05
  expect_equal(po$param_set$values, list(quantiles.q_vals = c(0.05, 0.5, 0.95), quantiles.q_response = 0.05, robust = TRUE))

  # Anfang der Error messages
  expect_error(PipeOpLearnerQuantiles$new(lrn, param_vals = list(quantiles.q_vals = c(0.75, 0.25, 0.5))), "sorted")
  expect_error(PipeOpLearnerQuantiles$new(lrn, param_vals = list(quantiles.q_vals = c(0.75, 0.25, NA))), "missing values")
  expect_error(PipeOpLearnerQuantiles$new(lrn, param_vals = list(quantiles.q_vals = -0.1)), ">= 0")
  expect_error(PipeOpLearnerQuantiles$new(lrn, param_vals = list(quantiles.q_vals = 1.1)), "<= 1")
  expect_error(PipeOpLearnerQuantiles$new(lrn, param_vals = list(quantiles.q_response = -1)))

  # q_response %in% q_vals
  task = mlr_tasks$get("mtcars")
  po_response = PipeOpLearnerQuantiles$new(lrn, param_vals = list(quantiles.q_vals = c(0.25, 0.75), quantiles.q_response = 0.5))
  expect_error(train_pipeop(po_response, list(task)), "additional elements")

  lrn_classif = mlr_learners$get("classif.featureless")
  expect_error(PipeOpLearnerQuantiles$new(lrn_classif), "only supports regression")
})

test_that("PipeOpLearnerQuantiles - graph but no id", {
  g = PipeOpNOP$new() %>>% PipeOpLearnerQuantiles$new(LearnerRegrDebug$new())
  po = PipeOpLearnerQuantiles$new(g)
  expect_string(po$id)
})

test_that("PipeOpLearnerQuantiles - model active binding to state", {
  lrn = mlr_learners$get("regr.debug")
  po = PipeOpLearnerQuantiles$new(lrn)
  task = mlr_tasks$get("mtcars")

  # before training states are NULL
  expect_null(po$state)
  expect_null(po$learner$state, po$state)
  expect_equal(po$learner, po$learner_model)
  expect_null(po$learner_model$state, po$state)

  train_out = po$train(list(task))
  train_state = po$state

  # after predicting states are unchanged
  predict_out = po$predict(list(task))
  expect_equal(po$state, train_state)
})

test_that("PipeOpLearnerQuantiles - predict_type is fixed", {
  lrn = lrn("regr.debug")
  po = PipeOpLearnerQuantiles$new(lrn)
  expect_equal(po$predict_type, c("response", "quantiles"))
})

test_that("PipeOpLearnerQuantiles - integration with larger graph", {
  task = mlr_tasks$get("mtcars")
  lrn = mlr_learners$get("regr.debug")

  po_quantiles = PipeOpLearnerQuantiles$new(lrn)
  po_nop = PipeOpNOP$new()
  graph = po_quantiles %>>% po_nop

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
  po = PipeOpLearnerQuantiles$new(lrn)
  po$train(list(task))
  po_state = po$state

  expect_class(po_state, "pipeop_learner_quantiles_state")

  po_state_marshaled = marshal_model(po_state, inplace = FALSE)

  expect_class(po_state_marshaled, "pipeop_learner_quantiles_state_marshaled")
  expect_true(is_marshaled_model(po_state_marshaled))

  po_state_unmarshaled = unmarshal_model(po_state_marshaled)
  expect_equal(po_state, po_state_unmarshaled)
})

test_that("marshal multiplicity", {
  lrn = lrn("regr.debug")
  lrn$properties = c(lrn$properties, "marshal")
  po = PipeOpLearnerQuantiles$new(lrn)

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
  po = PipeOpLearnerQuantiles$new(lrn)

  task1 = mlr_tasks$get("mtcars")
  task2 = mlr_tasks$get("boston_housing")

  input = Multiplicity(task1, task2)

  po$train(list(input))

  expect_class(po$state, "Multiplicity")
  expect_class(po$state[[1L]], "pipeop_learner_quantiles_state")
  expect_class(po$learner_model, "Multiplicity")
  expect_class(po$learner_model[[1L]][[1L]], "LearnerRegr")
  expect_equal(length(po$learner_model), length(input))
  expect_equal(length(po$learner_model[[1]]), length(po$param_set$values$quantiles.q_vals))

  prds = po$predict(list(input))
  expect_class(prds$output, "Multiplicity")
  expect_equal(length(prds$output), length(input))
  expect_class(prds$output[[1L]], "PredictionRegr")

  # recursive
  po1 = PipeOpLearnerQuantiles$new(lrn)
  po1$train(list(Multiplicity(input)))
  expect_class(po1$state, "Multiplicity")
  expect_class(po1$state[[1L]], "Multiplicity")
  expect_class(po1$state[[1L]][[1L]], "pipeop_learner_quantiles_state")
  expect_class(po1$learner_model, "Multiplicity")
  expect_class(po1$learner_model[[1L]], "Multiplicity")
  expect_class(po1$learner_model[[1L]][[1L]][[1]], "LearnerRegr")
  expect_equal(length(po1$learner_model[[1L]]), length(input))
  expect_equal(length(po1$learner_model[[1L]][[1L]]), length(po1$param_set$values$quantiles.q_vals))

  prds1 = po1$predict(list(Multiplicity(input)))
  expect_class(prds1$output, "Multiplicity")
  expect_class(prds1$output[[1L]], "Multiplicity")
  expect_class(prds1$output[[1L]][[1L]], "PredictionRegr")
  expect_equal(length(prds1$output[[1L]]), length(input))
})

