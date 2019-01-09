context("downsample")

test_that("PipeOpDownsample - basic properties", {
  op = PipeOpDownsample$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))
})

test_that("PipeOpDownsample - basic properties", {
  op = PipeOpDownsample$new()
  test_basic_pipeop_props(op)
})

test_that("PipeOpDownsample works unstratified", {
  task = mlr_tasks$get("iris")

  # Can be constructed
  op = PipeOpDownsample$new()
  expect_class(op, "PipeOpDownsample")
  expect_false(op$is_trained)

  # Can be trained
  tnew = op$train(list(task))
  expect_true(op$is_trained)
  expect_list(op$state, len = 0)
  expect_true(tnew$nrow == ceiling(task$nrow * 0.7))
})

test_that("PipeOpDownsample works stratified", {
  task = mlr_tasks$get("iris")

  # Can be constructed
  op = PipeOpDownsample$new()
  op$param_vals = list(stratify = TRUE, perc = 0.6)
  expect_class(op, "PipeOpDownsample")
  expect_false(op$is_trained)

  # Can be trained
  tnew = op$train(list(task))
  expect_true(op$is_trained)
  expect_list(op$state, len = 0)
  expect_true(tnew$nrow == ceiling(task$nrow * 0.6))
  # Proportions as expected
  expect_equal(table(tnew$data(cols = tnew$target_names)),
    table(rep(c("setosa", "versicolor", "virginica"), 30)))
})
