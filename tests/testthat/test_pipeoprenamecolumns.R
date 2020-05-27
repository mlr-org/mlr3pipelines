context("PipeOpRenameColumns")

test_that("PipeOpRenameColumns - basic properties", {
  task = mlr_tasks$get("iris")
  op = PipeOpRenameColumns$new()
  expect_pipeop(op)
  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]
  expect_equal(train_out, task)
  expect_equal(predict_out, task)
  expect_datapreproc_pipeop_class(PipeOpRenameColumns, task = task, predict_like_train = TRUE)
})

test_that("PipeOpRenameColumns - renaming works", {
  task = mlr_tasks$get("iris")
  op = PipeOpRenameColumns$new(param_vals = list(old = c("Species", "Petal.Length"), new = c("S", "PL")))
  train_out1 = op$train(list(task))[[1L]]
  predict_out1 = op$predict(list(task))[[1L]]
  expect_equal(train_out1, predict_out1)
  expect_true("S" == train_out1$target_names)
  expect_true("PL" %in% train_out1$feature_names)
  expect_true("Petal.Length" %nin% train_out1$feature_names)
  expect_equivalent(train_out1$data(cols = c("S", "PL")), task$data(cols = c("Species", "Petal.Length")))

  col_roles = train_out1$col_roles
  col_roles$name = "Petal.Width"
  col_roles$weight = "Sepal.Length"
  col_roles$feature = setdiff(col_roles$feature, c("Petal.Width", "Sepal.Length"))
  op$param_set$values = list(old = c("Petal.Width", "Sepal.Length", "Sepal.Width"), new = c("PW", "SL", "SW"))
  train_out1$col_roles = col_roles
  train_out2 = op$train(list(train_out1))[[1L]]
  predict_out2 = op$predict(list(train_out1))[[1L]]
  expect_equal(train_out2, predict_out2)
  expect_true(all(c("PL", "SW") == train_out2$col_roles$feature))
  expect_true("S" == train_out2$col_roles$target)
  expect_true("PW" == train_out2$col_roles$name)
  expect_true("SL" == train_out2$col_roles$weight)
  expect_equivalent(train_out1$data(), train_out2$data())
})

test_that("PipeOpRenameColumns - error handling", {
  task = mlr_tasks$get("iris")
  op = PipeOpRenameColumns$new(param_vals = list(new = "Test"))
  expect_error(op$train(list(task)), regexp = "length of new")
})
