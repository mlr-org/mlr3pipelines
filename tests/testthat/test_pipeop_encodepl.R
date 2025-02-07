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

  # Test default parameters
  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # bins are correct
  expect_equal(op$state$bins, list(x = c(2, 7, 12), y = c(10, 50, 95)))

  # encoding and column naming
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

  # bins are correct
  expect_equal(op$state$bins, list(x = c(2, 4, 8, 12), y = c(10, 25, 60, 95)))

  # encoding and column naming
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
  expect_error(PipeOpEncodePLTree$new(task_type = "TaskBla"), "Must be element of")

  # error for not supported task type
  expect_error(PipeOpEncodePLTree$new(task_type = "TaskUnsupervised"), "not supported")
})

test_that("PipeOpEncodePLTree - TaskRegr train and predict", {
  skip_if_not_installed("rpart")

  op = PipeOpEncodePLTree$new(task_type = "TaskRegr")
  dt = data.table(
    target = c(rep(c(5, 10, 15), each = 15), NA),
    x = c(seq(1, 45), NA),
    y = c(rep(c(1, 2, 4), each = 15), NA)
  )
  task = TaskRegr$new(id = "test", backend = dt, target = "target")

  # Test default parameters
  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # bins are correct
  expect_equal(op$state$bins, list(x = c(1, 15.5, 30.5, 45), y = c(1, 1.5, 3, 4)))

  # encoding and column naming
  dt_encoded = data.table(
    x.bin1 = c(seq(0, 14)/14.5, rep(1, 30), NA),
    x.bin2 = c(rep(0, 15), seq(0.5, 14.5)/15, rep(1, 15), NA),
    x.bin3 = c(rep(0, 30), seq(0.5, 14.5)/14.5, NA),
    y.bin1 = c(rep(0, 15), rep(1, 30), NA),
    y.bin2 = c(rep(0, 15), rep(0.5/1.5, 15), rep(1, 15), NA),
    y.bin3 = c(rep(0, 30), rep(1, 15), NA)
  )
  expect_equal(train_out$data(cols = train_out$feature_names), dt_encoded)
  expect_equal(predict_out$data(cols = predict_out$feature_names), dt_encoded)

  # Test task with no splits
  dt = data.table(
    target = rep(1, 45),
    x = seq(1, 45)
  )
  task_nosplit = TaskRegr$new(id = "nosplit", backend = dt, target = "target")

  train_out = op$train(list(task_nosplit))[[1L]]
  predict_out = op$predict(list(task_nosplit))[[1L]]

  # bins are correct
  expect_equal(op$state$bins, list(x = c(1, 45)))

  # encoding and column naming
  dt_encoded = data.table(x.bin1 = seq(0, 44)/44)
  expect_equal(train_out$data(cols = train_out$feature_names), dt_encoded)
  expect_equal(predict_out$data(cols = predict_out$feature_names), dt_encoded)

  # Changed param vals
  op$param_set$set_values(minsplit = 40)

  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # bins are correct
  expect_equal(op$state$bins, list(x = c(1, 15.5, 45), y = c(1, 1.5, 4)))

  # encoding and column naming
  dt_encoded = data.table(
    x.bin1 = c(seq(0, 14)/14.5, rep(1, 30), NA),
    x.bin2 = c(rep(0, 15), seq(0.5, 29.5)/29.5, NA),
    y.bin1 = c(rep(0, 15), rep(1, 30), NA),
    y.bin2 = c(rep(0, 15), rep(0.5/2.5, 15), rep(1, 15), NA)
  )
  expect_equal(train_out$data(cols = train_out$feature_names), dt_encoded)
  expect_equal(predict_out$data(cols = predict_out$feature_names), dt_encoded)
})

test_that("PipeOpEncodePLTree - TaskClassif train and predict", {
  skip_if_not_installed("rpart")

  op = PipeOpEncodePLTree$new(task_type = "TaskClassif")
  dt = data.table(
    target = as.factor(c(rep(c("A", "B", "C"), each = 15), NA)),
    x = c(seq(1, 45), NA),
    y = c(rep(c(1, 2, 4), each = 15), NA)
  )
  task = TaskClassif$new(id = "test", backend = dt, target = "target")

  # Test default parameters
  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # bins are correct
  expect_equal(op$state$bins, list(x = c(1, 15.5, 30.5, 45), y = c(1, 1.5, 3, 4)))

  # encoding and column naming
  dt_encoded = data.table(
    x.bin1 = c(seq(0, 14)/14.5, rep(1, 30), NA),
    x.bin2 = c(rep(0, 15), seq(0.5, 14.5)/15, rep(1, 15), NA),
    x.bin3 = c(rep(0, 30), seq(0.5, 14.5)/14.5, NA),
    y.bin1 = c(rep(0, 15), rep(1, 30), NA),
    y.bin2 = c(rep(0, 15), rep(0.5/1.5, 15), rep(1, 15), NA),
    y.bin3 = c(rep(0, 30), rep(1, 15), NA)
  )
  expect_equal(train_out$data(cols = train_out$feature_names), dt_encoded)
  expect_equal(predict_out$data(cols = predict_out$feature_names), dt_encoded)

  # Test task with no splits
  dt = data.table(
    target = as.factor(rep(c("A", "B"), each = 5)),
    x = rep(1, 10)
  )
  task_nosplit = TaskClassif$new(id = "nosplit", backend = dt, target = "target")

  train_out = op$train(list(task_nosplit))[[1L]]
  predict_out = op$predict(list(task_nosplit))[[1L]]

  # bins are correct
  expect_equal(op$state$bins, list(x = c(1, 1)))

  # encoding and column naming
  dt_encoded = data.table(x.bin1 = rep(1, 10))
  expect_equal(train_out$data(cols = train_out$feature_names), dt_encoded)
  expect_equal(predict_out$data(cols = predict_out$feature_names), dt_encoded)

  # Changed param vals
  op$param_set$set_values(minsplit = 40)

  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  # bins are correct
  expect_equal(op$state$bins, list(x = c(1, 15.5, 45), y = c(1, 1.5, 4)))

  # encoding and column naming
  dt_encoded = data.table(
    x.bin1 = c(seq(0, 14)/14.5, rep(1, 30), NA),
    x.bin2 = c(rep(0, 15), seq(0.5, 29.5)/29.5, NA),
    y.bin1 = c(rep(0, 15), rep(1, 30), NA),
    y.bin2 = c(rep(0, 15), rep(0.5/2.5, 15), rep(1, 15), NA)
  )
  expect_equal(train_out$data(cols = train_out$feature_names), dt_encoded)
  expect_equal(predict_out$data(cols = predict_out$feature_names), dt_encoded)
})
