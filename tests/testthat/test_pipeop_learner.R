context("PipeOpLearner")

test_that("PipeOpLearner - basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearner$new(lrn)
  expect_pipeop(po, check_ps_default_values = FALSE)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  task = mlr_tasks$get("iris")
  result = train_pipeop(po, list(task = task))
  expect_null(result[[1L]])

  result = predict_pipeop(po, list(task = task))
  expect_class(result[[1L]], "Prediction")

  expect_pipeop_class(PipeOpLearner, list(lrn), check_ps_default_values = FALSE)
  expect_error(PipeOpLearner$new())
})

test_that("PipeOpLearner - param_set and values", {
  skip_if_not_installed("rpart")
  lrn = mlr_learners$get("classif.rpart")
  po = PipeOpLearner$new(lrn)

  # Setting and getting pipeops works
  expect_pipeop(po, check_ps_default_values = FALSE)
  expect_equal(po$param_set, po$learner$param_set)


  expect_equal(po$param_set$values, po$learner$param_set$values)
  expect_error({
    po$param_set$values$minsplit = "foo"
  })
  po$param_set$values$minsplit = 2L
  expect_equal(po$param_set$values, po$learner$param_set$values)

  sortnames = function(x) {
    if (!is.null(names(x))) {
      x <- x[order(names(x), decreasing = TRUE)]
    }
    x
  }

  expect_equal(sortnames(po$param_set$values), list(xval = 0L, minsplit = 2L))
  po$param_set$values$maxdepth = 1L
  expect_equal(sortnames(po$param_set$values), list(xval = 0L, minsplit = 2L, maxdepth = 1L))
  po$param_set$values = list(minsplit = 1L)
  expect_equal(po$param_set$values, list(minsplit = 1L))
  expect_error({
    po$param_set$values = list(minsplit = "foo")
  })
  expect_error({
    po$param_set$values = list(foo = "foo")
  })
})

test_that("PipeOpLearner - graph but no id", {
  skip_if_not_installed("rpart")
  g = PipeOpNOP$new() %>>% PipeOpLearner$new(LearnerClassifRpart$new())
  po = PipeOpLearner$new(g)
  expect_string(po$id)
})

test_that("PipeOpLearner - model active binding to state", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearner$new(lrn)
  task = mlr_tasks$get("iris")

  # before training states are NULL
  expect_null(po$state)
  expect_equal(po$learner$state, po$state)
  expect_equal(po$learner_model$state, po$state)

  # after training learner_model's state and state are equal
  train_out = po$train(list(task))
  train_state = po$state
  expect_null(po$learner$state)
  expect_equal(po$learner_model$state, train_state)

  # after predicting states are unchanged
  predict_out = po$predict(list(task))
  expect_equal(po$state, train_state)
  expect_null(po$learner$state)
  expect_equal(po$learner_model$state, po$state)
})

test_that("packages", {
  skip_if_not_installed("rpart")

  expect_set_equal(
    c("mlr3pipelines", lrn("classif.rpart")$packages),
    po("learner", learner = lrn("classif.rpart"))$packages
  )
})

test_that("marshal", {
  task = tsk("iris")
  po_lrn = as_pipeop(lrn("classif.debug"))
  po_lrn$train(list(task))
  po_state = po_lrn$state
  expect_class(po_state, "learner_state")
  po_state_marshaled = marshal_model(po_state, inplace = FALSE)
  expect_class(po_state_marshaled, "learner_state_marshaled")
  expect_true(is_marshaled_model(po_state_marshaled))
  expect_equal(po_state, unmarshal_model(po_state_marshaled))
})

test_that("multiple marshal round-trips", {
  task = tsk("iris")
  glrn = as_learner(as_graph(lrn("classif.debug")))
  glrn$train(task)
  glrn$marshal()$unmarshal()$marshal()$unmarshal()
  expect_class(glrn$model, "graph_learner_model")
  expect_class(glrn$model$classif.debug$model, "classif.debug_model")

  expect_learner(glrn, task = task)
})

test_that("marshal multiplicity", {
  po = as_pipeop(lrn("classif.debug"))
  po$train(list(Multiplicity(tsk("iris"), tsk("sonar"))))
  s = po$state
  sm = marshal_model(po$state)
  expect_class(po$state, "Multiplicity")
  expect_true(is_marshaled_model(sm$marshaled[[1L]]))
  expect_true(is_marshaled_model(sm$marshaled[[2L]]))

  su = unmarshal_model(sm)
  expect_equal(su, s)

  # recursive
  po = as_pipeop(lrn("classif.debug"))
  po$train(list(Multiplicity(Multiplicity(tsk("iris")))))
  p1 = po$predict(list(Multiplicity(Multiplicity(tsk("iris")))))

  s = po$state
  sm = marshal_model(po$state)
  expect_class(po$state, "Multiplicity")
  expect_true(is_marshaled_model(sm$marshaled[[1L]][[1L]]))

  su = unmarshal_model(sm)
  expect_equal(su, s)

  po$state = su
  p2 = po$predict(list(Multiplicity(Multiplicity(tsk("iris")))))
  expect_equal(p1, p2)

  task = tsk("iris")
  glrn = as_learner(as_pipeop(lrn("classif.debug")))
  expect_learner(glrn, task)
  p1 = glrn$train(task)$predict(task)
  s1 = glrn$state
  glrn$marshal()$unmarshal()
  s2 = glrn$state
  p2 = glrn$predict(task)
  expect_equal(p1, p2)
  expect_equal(s1, s2)

  # recursive but nothing to do
  learner = as_learner(as_pipeop(lrn("regr.debug")))
  learner$train(tsk("mtcars"))
  learner$marshal()
  # nothing needed marshaling
  expect_false(learner$marshaled)
})

test_that("state class and multiplicity", {
  po = as_pipeop(lrn("classif.debug"))
  po$train(list(Multiplicity(tsk("iris"))))
  expect_class(po$state, "Multiplicity")
  expect_class(po$state[[1L]], "learner_state")

  # recursive
  po1 = as_pipeop(lrn("classif.debug"))
  po1$train(list(Multiplicity(Multiplicity(tsk("iris")))))
  expect_class(po1$state, "Multiplicity")
  expect_class(po1$state[[1L]], "Multiplicity")
  expect_class(po1$state[[1L]][[1L]], "learner_state")
})
