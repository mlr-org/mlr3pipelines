context("PipeOpUndersample")

test_that("PipeOpUndersample - basic properties", {
  op = PipeOpUndersample$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))

  expect_datapreproc_pipeop_class(PipeOpDownsample, task = task,
    predict_like_train = FALSE, deterministic_train = FALSE)
})

test_that("PipeOpUndersample", {
  op = PipeOpUndersample$new()
  op$param_set$values = list(frac = 0.5)
  task = mlr_tasks$get("pima")
  nt = op$train(list(task))[[1L]]

  expect_true(table(task$truth())[["pos"]] == table(nt$truth())[["pos"]])
  expect_true(table(task$truth())[["neg"]] >  table(nt$truth())[["neg"]])
})


test_that("PipeOpUndersample: rate and multiple classes", {
  task = mlr_tasks$get("zoo")
  op = PipeOpUndersample$new()
  table(op$train(list(task))[[1L]]$truth())
  nt = op$train(list(task))[[1L]]
  expect_equal(table(nt$truth())[["mammal"]], 10L)
})
