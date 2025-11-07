context("PipeOpTomek")

test_that("PipeOpTomek - basic properties", {
  skip_if_not_installed("themis")

  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpTomek, task = task, predict_like_train = FALSE)

})

test_that("PipeOpTomek - train works as intended", {
  skip_if_not_installed("themis")

  op = PipeOpTomek$new()

  # Compare to themis::tomek for task with only numeric features
  task = mlr_tasks$get("iris")
  train_out = op$train(list(task))[[1]]$data()
  tomek_out = setDT(invoke(themis::tomek, df = task$data(), var = task$target_names))

  expect_equal(train_out, tomek_out)

  # Compare to themis::tomek for task with uncommon row_ids
  task$filter(51:150)
  train_out = op$train(list(task))[[1]]$data()
  tomek_out = setDT(invoke(themis::tomek, df = task$data(), var = task$target_names))

  expect_equal(train_out, tomek_out)

  # Compare to themis::tomek for task with other features types (which should be ignored)
  task = mlr_tasks$get("german_credit")
  train_out = op$train(list(task))[[1]]$data()
  dt = task$data(cols = c(task$feature_types[type %in% c("integer", "numeric"), id], task$target_names))
  dt_out = setDT(invoke(themis::tomek, df = dt, var = task$target_names))
  tomek_out = task$data()[dt_out, on = colnames(dt_out)]

  expect_equal(train_out, tomek_out)

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
