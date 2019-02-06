context("PipeOpLearner")

test_that("PipeOLearner - basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearner$new(lrn)
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 1)
  expect_data_table(po$output, nrow = 1)

  task = mlr_tasks$get("iris")
  result = train_pipeop(po, list(task = task))
  expect_null(result[[1L]])

  result = predict_pipeop(po, list(task = task))
  expect_class(result[[1L]], "Prediction")

  expect_pipeop_class(PipeOpLearner, list(lrn))
  expect_error(PipeOpLearner$new())

})


test_that("PipeOLearner - param_set and param_vals", {
  lrn = mlr_learners$get("classif.rpart")
  po = PipeOpLearner$new(lrn)

  # Setting and getting pipeops works
  expect_pipeop(po)
  expect_equal(po$param_set, po$learner$param_set)
  expect_equal(po$param_set$param_vals, po$learner$param_set$param_vals)
  expect_error({po$param_set$param_vals$minsplit = "foo"})
  po$param_set$param_vals$minsplit = 2L
  expect_equal(po$param_set$param_vals, po$learner$param_set$param_vals)
  expect_equal(po$param_set$param_vals, list(minsplit = 2L))
  po$param_set$param_vals$maxdepth = 1L
  expect_equal(po$param_set$param_vals, list(minsplit = 2L, maxdepth = 1L))
  po$param_set$param_vals = list(minsplit = 1L)
  expect_equal(po$param_set$param_vals, list(minsplit = 1L))
  expect_error({po$param_set$param_vals = list(minsplit = "foo")})
  expect_error({po$param_set$param_vals = list(foo = "foo")})
})
