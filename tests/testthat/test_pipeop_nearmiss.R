context("PipeOpNearmiss")

test_that("PipeOpNearmiss - basic properties", {
  skip_if_not_installed("themis")

  task = mlr_tasks$get("wine")
  expect_datapreproc_pipeop_class(PipeOpNearmiss, task = task, predict_like_train = FALSE)

})

test_that("PipeOpNearmiss - train works as intended", {
  skip_if_not_installed("themis")

  op = PipeOpNearmiss$new()

  # Compare to themis::nearmiss for task with only numeric/integer features
  task = mlr_tasks$get("wine")
  train_out = op$train(list(task))[[1]]$data()
  nearmiss_out = setDT(invoke(themis::nearmiss, df = task$data(), var = task$target_names))

  expect_equal(train_out, nearmiss_out)

  # Compare to themis::nearmiss for task with other features types (which should be ignored)
  task = mlr_tasks$get("german_credit")
  train_out = op$train(list(task))[[1]]$data()
  dt = task$data(cols = c(task$feature_types[type %in% c("integer", "numeric"), id], task$target_names))
  dt_out = setDT(invoke(themis::nearmiss, df = dt, var = task$target_names))
  nearmiss_out = task$data()[dt_out, on = colnames(dt_out)]

  expect_equal(train_out, nearmiss_out)

  # Compare to themis::nearmiss with changed params
  task = mlr_tasks$get("wine")
  op$param_set$set_values(k = 8, under_ratio = 0.9)
  train_out = op$train(list(task))[[1]]$data()
  nearmiss_out = setDT(invoke(themis::nearmiss, df = task$data(), var = task$target_names,
                       k = 8, under_ratio = 0.9))

  expect_equal(train_out, nearmiss_out)

  op$param_set$set_values(k = 8, under_ratio = 1.1)
  train_out = op$train(list(task))[[1]]$data()
  nearmiss_out = setDT(invoke(themis::nearmiss, df = task$data(), var = task$target_names,
                       k = 8, under_ratio = 1.1))

  expect_equal(train_out, nearmiss_out)

  # Empty task is returned unchanged
  task$select(character(0))
  expect_equal(
    op$train(list(task))[[1L]],
    task
  )

  # PipeOp does not accept tasks that have no integer or numeric feature
  task = tsk("breast_cancer")
  expect_error(op$train(list(task)))

})
