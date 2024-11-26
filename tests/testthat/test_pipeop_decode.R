context("PipeOpDecode")

test_that("PipeOpDecode - basic properties", {
  dt = data.table(
    target = runif(120),
    x.1 = rep(c(1, 0, 0), 40),
    x.2 = rep(c(0, 1, 0), 40),
    x.3 = rep(c(0, 0, 1), 40),
    y.1 = rep(c(1, 0, 0), 40),
    y.2 = rep(c(0, 1, 0), 40),
    y.3 = rep(c(0, 0, 1), 40),
    a = runif(120)
  )
  task = TaskRegr$new(id = "decode", backend = dt, target = "target")

  expect_datapreproc_pipeop_class(PipeOpDecode, task = task, deterministic_train = FALSE, deterministic_predict = FALSE)
})

test_that("PipeOpDecode - one-hot-encoding", {
  op = PipeOpDecode$new()

  dt = data.frame(
    target = runif(10),
    x.1 = rep(c(1, 0), 5),
    x.2 = rep(c(0, 1), 5),
    .a = runif(10),
    a = runif(10)
  )
  task = TaskRegr$new(id = "decode", backend = dt, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = dt$target,
    .a = dt$.a,
    a = dt$a,
    x = as.factor(rep(c(1, 2), times = 5))
  )
  expect_equal(train_out, dt_compare)

  # can handle task with no matches to group_pattern
  dt = data.table(
    target = runif(10),
    a = runif(10)
  )
  task = TaskRegr$new(id = "decode", backend = dt, target = "target")
  train_out = op$train(list(task))[[1]]$data()
  expect_equal(train_out, dt)

  # test tiebreak
})

test_that("PipeOpDecode - treatment encoding", {
  op = PipeOpDecode$new()
  op$param_set$values$treatment_encoding = TRUE

  dt = data.table(
    target = runif(15),
    x.1 = rep(c(1, 0, 0), 5),
    x.2 = rep(c(0, 0, 1), 5),
    a = runif(15)
  )
  task = TaskRegr$new(id = "decode", backend = dt, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = df$target,
    a = df$a,
    x = rep(c("1", "ref", "2"), times = 5)
  )
  expect_equal(train_out, dt_compare)

  # test cutoff
  # test tiebreak
})

test_that("PipOpDecode - collapse all into one", {
  op = PipeOpDecode$new()
  op$param_set$values$group_pattern = ""

  dt = data.frame(
    target = runif(15),
    x.1 = rep(c(1, 0, 0), 5),
    x.2 = rep(c(0, 0, 1), 5),
    a = rep(c(0, 1, 0), 5)
  )
  task = TaskRegr$new(id = "decode", backend = dt, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = dt$target,
    pipeop.decoded = as.factor(rep(c("x.1", "a", "x.2"), times = 5))
  )
  expect_equal(train_out, dt_compare)
})

test_that("PipeOpDecode - errors", {
  op = PipeOpDecode$new()
  dt = data.frame(
    target = runif(20),
    x.1 = rep(c(1, 0), 10),
    x.2 = rep(c(0, 1), 10),
    .a = rep(1, 20)
  )
  task = TaskRegr$new(id = "decode", backend = dt, target = "target")

  # pattern without capturing group
  op$param_set$values$group_pattern = "^[^.]+\\."
  expect_error(op$train(list(task)), "nothing was captured")

  # pattern that would result in empty column names
  op$param_set$values$group_pattern = "^([^.]*)\\."
  expect_error(op$train(list(task)), "produce empty string as decoded column name")
})
