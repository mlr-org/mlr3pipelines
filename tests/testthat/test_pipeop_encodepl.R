context("PipeOpEncodePL")

test_that("PipeOpEncodePLQuantiles - basic properties", {
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpEncodePLQuantiles, task = task)
})

test_that("PipeOpEncodePLQuantiles - train and predict", {
  op = PipeOpEncodePLQuantiles$new()
  dt = data.table(
    target = as.factor(rep(c("A", "B"), 4)),
    x = c(2, 8, 4, 10, 6, 12, NA, NA),
    y = c(NA, NA, 10, 25, 40, 60, 65, 95)
  )
  task = TaskClassif$new(id = "test", backend = dt, target = "target")

  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # Test that bins are correct
  expect_equal(op$state$bins, list(x = c(2, 7, 12), y = c(10, 50, 95)))

  # Test encoding and column naming
  dt_encoded = data.table(
    x.bin1 = c(0, 1, 2/5, 1, 4/5, 1, NA, NA),
    x.bin2 = c(0, 1/5, 0, 3/5, 0, 1, NA, NA),
    y.bin1 = c(NA, NA, 0, 15/40, 3/4, 1, 1, 1),
    y.bin2 = c(NA, NA, 0, 0, 0, 10/45, 15/45, 1)
  )
  expect_equal(train_out$data(cols = train_out$feature_names), dt_encoded)
  expect_equal(predict_out$data(cols = predict_out$feature_names), dt_encoded)

  # Changed param values
  op$param_set$set_values(numsplits = 3, type = 1)

  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # Test that bins are correct
  expect_equal(op$state$bins, list(x = c(2, 4, 8, 12), y = c(10, 25, 60, 95)))

  # Test encoding and column naming
  dt_encoded = data.table(
    x.bin1 = c(0, 1, 1, 1, 1, 1, NA, NA),
    x.bin2 = c(0, 1, 0, 1, 0.5, 1, NA, NA),
    x.bin3 = c(0, 0, 0, 0.5, 0, 1, NA, NA),
    y.bin1 = c(NA, NA, 0, 1, 1, 1, 1, 1),
    y.bin2 = c(NA, NA, 0, 0, 15/35, 1, 1, 1),
    y.bin3 = c(NA, NA, 0, 0, 0, 0, 5/35, 1)
  )
  expect_equal(train_out$data(cols = train_out$feature_names), dt_encoded)
  expect_equal(predict_out$data(cols = predict_out$feature_names), dt_encoded)
})

test_that("PipeOpEncodePLTree - basic properties", {
  skip_if_not_installed("rpart")

  task = mlr_tasks$get("mtcars")
  expect_datapreproc_pipeop_class(PipeOpEncodePLTree, constargs = list(task_type = "TaskRegr"), task = task)

  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpEncodePLTree, constargs = list(task_type = "TaskClassif"), task = task)

  # error for non-existing task type

  # error for not supported task type

})

test_that("PipeOpEncodePLTree - TaskRegr train and predict", {
  skip_if_not_installed("rpart")

  op = PipeOpEncodePLTree$new(task_type = "TaskRegr")
  dt = data.table(
    target = c(1.5, 20, 3.5, 30.8, 14.3, 90.4),
    x = c(2, 8, 4, 10, 6, 12),
    y = c(10, 25, 40, 60, 65, 95)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")

  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # Test that bins are correct
  #expect_equal(op$state$bins, list(x = c(), y = c()))

  # Test encoding and column naming

  # Changed param vals


  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # Test that bins are correct

  # Test encoding and column naming

  # Cannot handle NAs in feature nor in target
  task_nafeature = task$clone(deep = TRUE)$cbind(data.frame(target = 0, x = NA, y = 0))
  expect_error(op$train(list(task_nafeature)), "")
  expect_error(op$predict(list(task_nafeature)), "")
  task_natarget = task$clone(deep = TRUE)$cbind(data.frame(target = NA, x = 0, y = 0))
  expect_error(op$train(list(task_nafeature)), "")
  expect_error(op$predict(list(task_nafeature)), "")
})

test_that("PipeOpEncodePLTree - TaskClassif train and predict", {
  skip_if_not_installed("rpart")

  op = PipeOpEncodePLTree$new(task_type = "TaskClassif")
  dt = data.table(
    target = rep(c("A", "B"), 3),
    x = c(2, 8, 4, 10, 6, 12),
    y = c(10, 25, 40, 60, 65, 95)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")

  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # Test that bins are correct
  #expect_equal(op$state$bins, list(x = c(), y = c()))

  # Test encoding and column naming

  # Changed param vals

  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # Test that bins are correct

  # Test encoding and column naming

  # Cannot handle NAs in feature nor in target
  task_nafeature = task$clone(deep = TRUE)$cbind(data.frame(target = 0, x = NA, y = 0))
  expect_error(op$train(list(task_nafeature)), "")
  expect_error(op$predict(list(task_nafeature)), "")
  task_natarget = task$clone(deep = TRUE)$cbind(data.frame(target = NA, x = 0, y = 0))
  expect_error(op$train(list(task_nafeature)), "")
  expect_error(op$predict(list(task_nafeature)), "")
})
