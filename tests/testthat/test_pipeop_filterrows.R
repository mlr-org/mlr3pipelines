context("PipeOpFilterRows")

test_that("PipeOpFilterRows - basic properties", {
  op = PipeOpFilterRows$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  expect_equal(train_pipeop(op, inputs = list(task))[[1L]], task)
  expect_equal(predict_pipeop(op, inputs = list(task))[[1L]], task)

  expect_datapreproc_pipeop_class(PipeOpFilterRows, task = task)

  expect_error(PipeOpFilterRows$new(param_vals = list(filter = list())))
})

test_that("PipeOpFilterRows - NA handling", {
  dat = iris
  dat$Sepal.Length[c(1L, 3L, 100L)] = NA
  dat$Petal.Width[c(1L, 4L)] = NA
  dat$Petal.Length[5] = NA
  dat$Species[2L] = NA
  task = TaskClassif$new("test", backend = dat, target = "Species")

  op = PipeOpFilterRows$new(param_vals = list(na_column = "Species"))

  op$param_set$values$na_column = "Sepal.Length"
  train_out1 = op$train(list(task))[[1L]]
  expect_equal(op$state$na_ids, c(1, 3, 100))

  op$param_set$values$na_column = "_all_"
  op$param_set$values$skip_during_predict = FALSE
  train_out2 = op$train(list(task))[[1L]]
  expect_equal(op$state$na_ids, c(1, 2, 3, 4, 5, 100))
  predict_out2 = op$predict(list(task))[[1L]]
  expect_equal(train_out2, predict_out2)

  op$param_set$values$skip_during_predict = TRUE
  expect_equal(op$predict(list(task))[[1L]], task)
})

test_that("PipeOpFilterRows - filter by column name", {
  set.seed(1)
  dat = iris
  dat$filter = sample(c(FALSE, TRUE), size = 150, replace = TRUE)
  task = TaskClassif$new("test", backend = dat, target = "Species")

  op = PipeOpFilterRows$new(param_vals = list(filter = "filter"))

  train_out1 = op$train(list(task))[[1L]]
  expect_equal(op$state$row_ids, which(dat$filter))
  expect_equal(train_out1$data(), task$data(which(dat$filter)))

  op$param_set$values$invert = TRUE
  train_out2 = op$train(list(task))[[1L]]
  expect_equal(op$state$row_ids, which(!dat$filter))
  expect_equal(train_out2$data(), task$data(which(!dat$filter)))
})

test_that("PipeOpFilterRows - filter by expression", {
  task = mlr_tasks$get("iris")
  task_cp = task$clone(deep = TRUE)

  op = PipeOpFilterRows$new(param_vals = list(filter = expression(Sepal.Length < 6 & Petal.Width > 1)))
  train_out1 = op$train(list(task))[[1L]]
  expect_equal(op$state$row_ids, which(iris$Sepal.Length < 6 & iris$Petal.Width > 1))

  # zero indices left
  op$param_set$values$filter = expression(Petal.Length < 3 & Petal.Width > 1)
  train_out2 = op$train(list(task))[[1L]]
  expect_equal(op$state$row_ids, integer(0))
  expect_equal(train_out2, task_cp$filter(integer(0)))
  op$param_set$values$invert = TRUE
  train_out3 = op$train(list(task))[[1L]]
  expect_equal(op$state$row_ids, 1:150)
  expect_equal(train_out3, task)
})

test_that("PipeOpFilterRows - filter by ids", {
  # in combination with NA
  dat = iris
  dat$Sepal.Length[c(1L, 3L, 100L)] = NA
  dat$Petal.Width[c(1L, 4L)] = NA
  dat$Petal.Length[5] = NA
  dat$Species[2L] = NA
  task = TaskClassif$new("test", backend = dat, target = "Species")

  op = PipeOpFilterRows$new(param_vals = list(na_column = "_all_", filter = 1:10))
  train_out1 = op$train(list(task))[[1L]]
  expect_equal(op$state$row_ids, setdiff(1:10, c(1, 2, 3, 4, 5)))
  op$param_set$values$invert = TRUE
  train_out2 = op$train(list(task))[[1L]]
  expect_equal(op$state$na_ids, c(1, 2, 3, 4, 5, 100))
  expect_true(!(100 %in% op$state$row_ids))
})

test_that("PipeOpFilterRows - use case iris", {
  task = mlr_tasks$get("iris")

  g = PipeOpFilterRows$new(param_vals = list(filter = expression(Sepal.Length < median(Sepal.Length)))) %>>%
    PipeOpLearnerCV$new(LearnerClassifRpart$new())

  train_out = g$train(task)[[1L]]
  predict_out = g$predict(task)[[1L]]
  expect_equal(g$state$filterrows$row_ids, which(with(task$data(), Sepal.Length < median(Sepal.Length))))
  expect_equal(g$state$filterrows$row_ids, train_out$row_ids)
  expect_equal(task$row_ids, predict_out$row_ids)
})
