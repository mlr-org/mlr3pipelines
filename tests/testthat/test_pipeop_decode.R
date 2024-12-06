context("PipeOpDecode")

test_that("PipeOpDecode - basic properties", {
  dt = data.table(
    target = runif(12),
    x.1 = rep(c(1, 0, 0), 4),
    x.2 = rep(c(0, 1, 0), 4),
    y.1 = rep(c(1, 0, 0), 4),
    y.2 = rep(c(0, 1, 0), 4),
    a = runif(12)
  )
  task = TaskRegr$new(id = "decode", backend = dt, target = "target")

  expect_datapreproc_pipeop_class(PipeOpDecode, task = task, deterministic_train = FALSE, deterministic_predict = FALSE)
})

test_that("PipeOpDecode - one-hot-encoding", {
  op = PipeOpDecode$new()

  dt = data.table(
    target = runif(10),
    x.1 = rep(c(1, 0), 5),
    x.2 = rep(c(0, 1), 5),
    y.1 = rep(c(2, 1), 5),
    y.2 = rep(c(1, 2), 5),
    .a = runif(10),
    a = runif(10)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = dt$target,
    .a = dt$.a,
    a = dt$a,
    x = as.factor(rep(c(1, 2), times = 5)),
    y = as.factor(rep(c(1, 2), times = 5))
  )
  expect_equal(train_out, dt_compare)

  # can handle task with no matches to group_pattern
  dt = data.table(
    target = runif(10),
    a = runif(10)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")
  train_out = op$train(list(task))[[1]]$data()
  expect_equal(train_out, dt)

  # tiebreak
  dt = data.table(
    target = runif(10),
    x.1 = c(1, 0, 1, 0, 0),
    x.2 = c(0, 1, 0, 1, 1),
    x.3 = c(0, 0, 1, 1, 1)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")

  op$param_set$values$ties_method = "first"
  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = dt$target,
    x = as.factor(c(1, 2, 1, 2, 2))
  )
  expect_equal(train_out, dt_compare)

  op$param_set$values$ties_method = "last"
  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = dt$target,
    x = as.factor(c(1, 2, 3, 3, 3))
  )
  expect_equal(train_out, dt_compare)

  # no name collision
  op$param_set$values$group_pattern = "^(.+)\\."  # matches everything till last dot

  dt = data.table(
    target = runif(10),
    x.1 = rep(c(1, 0), 5),
    x.2 = rep(c(0, 1), 5),
    x.1.a = rep(c(2, 1), 5),
    x.1.b = rep(c(1, 2), 5)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = dt$target,
    x = as.factor(rep(c(1, 2), 5)),
    x.1 = as.factor(rep(c("a", "b"), 5))
  )
  expect_equal(train_out, dt_compare)

})

test_that("PipeOpDecode - treatment encoding", {
  op = PipeOpDecode$new()
  op$param_set$values$treatment_encoding = TRUE

  dt = data.table(
    target = runif(15),
    x.1 = rep(c(1, 0, 0), 5),
    x.2 = rep(c(0, 0, 0.5), 5),
    a = runif(15)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = dt$target,
    a = dt$a,
    x = as.factor(rep(c("1", "ref", "2"), times = 5))
  )
  expect_equal(train_out, dt_compare)

  # test cutoff
  op$param_set$values$treatment_cutoff = 0.5
  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = dt$target,
    a = dt$a,
    x = as.factor(rep(c("1", "ref", "ref"), times = 5))
  )
  expect_equal(train_out, dt_compare)
  op$param_set$values$treatment_cutoff = 0

  # test incrementing reference level name
  op$param_set$values$ref_name = "x"
  dt = data.table(
    target = runif(15),
    x.x = rep(c(1, 0, 0), 5),
    x.x.1 = rep(c(0, 0, 0.5), 5)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = dt$target,
    x = as.factor(rep(c("x", "x.2", "x.1"), times = 5))
  )
  expect_equal(train_out, dt_compare)

})

test_that("PipOpDecode - collapse all into one", {
  op = PipeOpDecode$new()
  op$param_set$values$group_pattern = ""

  dt = data.table(
    target = runif(15),
    x = rep(c(1, 0, 0), 5),
    y = rep(c(0, 1, 0), 5),
    z = rep(c(0, 0, 1), 5)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")

  train_out = op$train(list(task))[[1]]$data()
  dt_compare = data.table(
    target = dt$target,
    pipeop.decoded = as.factor(rep(c("x", "y", "z"), times = 5))
  )
  expect_equal(train_out, dt_compare)
})

test_that("PipeOpDecode - errors", {
  op = PipeOpDecode$new()
  dt = data.table(
    target = runif(20),
    x.1 = rep(c(1, 0), 10),
    x.2 = rep(c(0, 1), 10),
    .a = rep(1, 20)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")

  # pattern without capturing group
  op$param_set$values$group_pattern = "^[^.]+\\."
  expect_error(op$train(list(task)), "nothing was captured")

  # pattern that would result in empty column names
  op$param_set$values$group_pattern = "^([^.]*)\\."
  expect_error(op$train(list(task)), "produce empty string as decoded column name")
})
