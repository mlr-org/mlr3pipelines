context("PipeOpLearner")

test_that("PipeOpLearner - basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearner$new(lrn)
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  task = mlr_tasks$get("iris")
  result = train_pipeop(po, list(task = task))
  expect_null(result[[1L]])

  result = predict_pipeop(po, list(task = task))
  expect_class(result[[1L]], "Prediction")

  expect_pipeop_class(PipeOpLearner, list(lrn))
  expect_error(PipeOpLearner$new())
})


test_that("PipeOLearner - param_set and values", {
  lrn = mlr_learners$get("classif.rpart")
  po = PipeOpLearner$new(lrn)

  # Setting and getting pipeops works
  expect_pipeop(po)
  expect_equal(po$param_set, po$learner$param_set)


  expect_equal(po$param_set$values, po$learner$param_set$values)
  expect_error({
    po$param_set$values$minsplit = "foo"
  })
  po$param_set$values$minsplit = 2L
  expect_equal(po$param_set$values, po$learner$param_set$values)
  expect_equal(po$param_set$values, list(xval = 0L, minsplit = 2L))
  po$param_set$values$maxdepth = 1L
  expect_equal(po$param_set$values, list(xval = 0L, minsplit = 2L, maxdepth = 1L))
  po$param_set$values = list(minsplit = 1L)
  expect_equal(po$param_set$values, list(minsplit = 1L))
  expect_error({
    po$param_set$values = list(minsplit = "foo")
  })
  expect_error({
    po$param_set$values = list(foo = "foo")
  })
})
