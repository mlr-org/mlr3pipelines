context("PipeOpDecode")

test_that("PipeOpDecode - basic properties", {
  # check what expect_datapreproc_pipeop_class does, whether we need custom task here
  df = data.frame(
    target = runif(120),
    x.1 = rep(c(1, 0, 0), 40),
    x.2 = rep(c(0, 1, 0), 40),
    x.3 = rep(c(0, 0, 1), 40),
    y.1 = rep(c(1, 0, 0), 40),
    y.2 = rep(c(0, 1, 0), 40),
    y.3 = rep(c(0, 0, 1), 40),
    a = runif(120)
  )
  task = TaskRegr$new(id = "decode", backend = df, target = "target")

  expect_datapreproc_pipeop_class(PipeOpDecode, task = task)
})

test_that("PipeOpDecode - one-hot-encoding", {
  op = PipeOpDecode$new()

  df = data.frame(
    target = runif(10),
    x.1 = rep(c(1, 0), 5),
    x.2 = rep(c(0, 1), 5),
    a = runif(10)
  )
  task = TaskRegr$new(id = "decode", backend = df, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt = data.table(
    x = rep(c(1, 2), each = 5),
    a = df$a
  )
  expect_equal(train_out, dt)

})

test_that("PipeOpDecode - treatment encoding", {
  op = PipeOpDecode$new()
  op$param_set$values$treatment_encoding = TRUE

  df = data.frame(
    target = runif(15),
    x.1 = rep(c(1, 0, 0), 5),
    x.2 = rep(c(0, 0, 1), 5),
    a = runif(15)
  )
  task = TaskRegr$new(id = "decode", backend = df, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt = data.table(
    x = rep(c("1", "ref", "2"), times = 5),
    a = df$a
  )
  expect_equal(train_out, dt)
})

test_that("PipOpDecode - different regex patterns", {
  op = PipeOpDecode$new()
  op$param_set$values$regex_pattern = ""

  df = data.frame(
    target = runif(15),
    x.1 = rep(c(1, 0, 0), 5),
    x.2 = rep(c(0, 0, 1), 5),
    a = rep(c(0, 1, 0), 5)
  )
  task = TaskRegr$new(id = "decode", backend = df, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt = data.table(
    x = rep(c("x.1", "a", "x.2"), times = 5)
  )
  expect_equal(train_out, dt)
})

test_that("PipeOpDecode - errors", {
  op = PipeOpDecode$new()

  df = data.frame(
    target = runif(15),
    x.1 = rep(c(1, 0, 0), 5),
    x.2 = rep(c(0, 1, 1), 5),
    a = runif(15)
  )
  task = TaskRegr$new(id = "decode", backend = df, target = "target")

  expect_error(op$train(list(task))) # due to non-unique which.max

})
