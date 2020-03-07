context("PipeOpLearnerRepresentation")

test_that("Basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerRepresentation$new(lrn)
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  task = mlr_tasks$get("iris")
  expect_error(train_pipeop(po, list(task = task)))
})

test_that("For rpart", {
  skip_if_not(require("treeClust"))
  lrn = mlr_learners$get("classif.rpart")
  po = PipeOpLearnerRepresentation$new(lrn)
  expect_pipeop(po)
  task = mlr_tasks$get("iris")
  l = train_pipeop(po, list(task = task))[[1]]
  expect_task(l)
  expect_true(l$ncol == 2)
  expect_true(l$nrow == task$nrow)
  expect_factor(l$data(cols = l$feature_names)[[1]])
})

test_that("For xgboost", {
  skip_if_not(require("xgboost"))
  skip_if_not(require("mlr3learners"))
  lrn = mlr_learners$get("classif.xgboost")
  po = PipeOpLearnerRepresentation$new(lrn)
  expect_pipeop(po)
  task = mlr_tasks$get("iris")
  l = train_pipeop(po, list(task = task))[[1]]
  expect_task(l)
  expect_true(l$ncol == 2)
  expect_true(l$nrow == task$nrow)
  expect_data_table(l$data(cols = l$feature_names))
})

