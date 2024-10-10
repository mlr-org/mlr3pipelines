context("PipeOpRenameColumns")

test_that("basic properties", {
  task = mlr_tasks$get("iris")
  op = PipeOpRenameColumns$new()
  expect_pipeop(op)
  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]
  expect_equal(train_out, task)
  expect_equal(predict_out, task)
  expect_datapreproc_pipeop_class(PipeOpRenameColumns, task = task, predict_like_train = TRUE)
})

test_that("renaming works", {
  task = mlr_tasks$get("iris")
  task$cbind(data.table(Petal.Width = as.character(1:150)))  # need a char column that we can turn into the 'name'-col
  op = PipeOpRenameColumns$new(param_vals = list(renaming = c("Petal.Length" = "PL")))
  train_out1 = op$train(list(task))[[1L]]
  predict_out1 = op$predict(list(task))[[1L]]
  expect_equal(train_out1, predict_out1)
  expect_true("PL" %in% train_out1$feature_names)
  expect_true("Petal.Length" %nin% train_out1$feature_names)
  expect_equivalent(train_out1$data(cols = "PL"), task$data(cols = "Petal.Length"))

  col_roles = train_out1$col_roles
  col_roles$name = "Petal.Width"
  col_roles[[if ("weights_learner" %in% names(task)) "weights_learner" else "weight"]] = "Sepal.Length"
  col_roles$feature = setdiff(col_roles$feature, c("Petal.Width", "Sepal.Length"))
  op$param_set$values = list(renaming = c("Petal.Width" = "PW", "Sepal.Length" = "SL", "Sepal.Width" = "SW"), ignore_missing = FALSE)
  train_out1$col_roles = col_roles
  train_out2 = op$train(list(train_out1))[[1L]]
  predict_out2 = op$predict(list(train_out1))[[1L]]
  expect_equal(train_out2, predict_out2)
  expect_true(all(c("PL", "SW") == train_out2$col_roles$feature))
  expect_true("PW" == train_out2$col_roles$name)
  expect_true("SL" == train_out2$col_roles[[if ("weights_learner" %in% names(task)) "weights_learner" else "weight"]])
  expect_equivalent(train_out1$data(), train_out2$data())
})

test_that("error handling", {
  task = mlr_tasks$get("iris")
  op = PipeOpRenameColumns$new(param_vals = list(renaming = c("Test" = "Newtest")))
  expect_error(op$train(list(task)), "The names Test from.*were not found in the Task")
  op$param_set$values$ignore_missing = TRUE
  expect_equal(task$data(), op$train(list(task))[[1]]$data())
})
