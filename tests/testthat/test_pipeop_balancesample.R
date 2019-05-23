context("PipeOpBalanceSample")

test_that("PipeOpBalanceSample - basic properties", {
  op = PipeOpBalanceSample$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))

  expect_datapreproc_pipeop_class(PipeOpSubsample, task = task,
    predict_like_train = FALSE, deterministic_train = FALSE)
})

test_that("PipeOpBalanceSample", {
  op = PipeOpBalanceSample$new()
  task = mlr_tasks$get("pima")

  op$param_set$values = list(ratio = 0.5, reference = "major", adjust = "major", shuffle = TRUE)
  nt = op$train(list(task))[[1L]]
  expect_true(table(task$truth())[["pos"]] == table(nt$truth())[["pos"]])
  expect_true(table(task$truth())[["neg"]] > table(nt$truth())[["neg"]])

  op$param_set$values = list(ratio = 0.5, reference = "major", adjust = "minor", shuffle = TRUE)
  nt = op$train(list(task))[[1L]]
  expect_true(table(task$truth())[["pos"]] > table(nt$truth())[["pos"]])
  expect_true(table(task$truth())[["neg"]] == table(nt$truth())[["neg"]])

  op$param_set$values = list(ratio = 0.5, reference = "major", adjust = "all", shuffle = TRUE)
  nt = op$train(list(task))[[1L]]
  expect_true(table(task$truth())[["pos"]] > table(nt$truth())[["pos"]])
  expect_true(table(task$truth())[["neg"]] > table(nt$truth())[["neg"]])
})


test_that("PipeOpBalanceSample: rate and multiple classes", {
  task = mlr_tasks$get("zoo")
  op = PipeOpBalanceSample$new()
  intbl = table(task$truth())

  op$param_set$values$reference = "nonmajor"
  op$param_set$values$adjust = "downsample"
  nt = op$train(list(task))[[1L]]
  outtbl = intbl
  outtbl[outtbl > 10] = 10
  expect_equal(table(nt$truth()), outtbl)

  op$param_set$values$reference = "nonmajor"
  op$param_set$values$adjust = "upsample"
  nt = op$train(list(task))[[1L]]
  outtbl = intbl
  outtbl[outtbl < 10] = 10
  expect_equal(table(nt$truth()), outtbl)

  op$param_set$values$reference = "nonmajor"
  op$param_set$values$adjust = "major"
  nt = op$train(list(task))[[1L]]
  outtbl = intbl
  outtbl[which.max(outtbl)] = 10
  expect_equal(table(nt$truth()), outtbl)

  op$param_set$values$reference = "nonmajor"
  op$param_set$values$adjust = "minor"
  nt = op$train(list(task))[[1L]]
  outtbl = intbl
  outtbl[which.min(outtbl)] = 10
  expect_equal(table(nt$truth()), outtbl)

  op$param_set$values$reference = "one"
  op$param_set$values$adjust = "all"
  nt = op$train(list(task))[[1L]]
  outtbl = intbl
  outtbl[TRUE] = 1
  expect_equal(table(nt$truth()), outtbl)
})
