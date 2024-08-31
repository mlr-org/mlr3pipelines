context("PipeOpNearmiss")

test_that("PipeOpNearmiss - basic properties", {
  skip_if_not_installed("themis")

  task = mlr_tasks$get("wine")
  expect_datapreproc_pipeop_class(PipeOpNearmiss, task = task, predict_like_train = FALSE)

})

test_that("PipeOpNearmiss - train works as intended", {
  skip_if_not_installed("themis")

  op = PipeOpNearmiss$new()
  task = mlr_tasks$get("wine")

  # Compare to themis::nearmiss
  train_out = op$train(list(task))[[1]]$data()
  nearmiss_out = setDT(invoke(themis::nearmiss, df = task$data(), var = task$target_names))

  expect_equal(train_out, nearmiss_out)

  # Compare to themis::nearmiss with changed params
  op$param_set$set_values(k = 8, under_ratio = 0.5)
  train_out = op$train(list(task))[[1]]$data()
  nearmiss_out = setDT(invoke(themis::nearmiss, df = task$data(), var = task$target_names,
                       k = 8, under_ratio = 0.5))

  expect_equal(train_out, nearmiss_out)

  op$param_set$set_values(k = 8, under_ratio = 1.5)
  train_out = op$train(list(task))[[1]]$data()
  nearmiss_out = setDT(invoke(themis::nearmiss, df = task$data(), var = task$target_names,
                       k = 8, under_ratio = 1.5))

  expect_equal(train_out, nearmiss_out)

  # Empty task is returned unchanged
  task$select(character(0))
  expect_equal(
    op$train(list(task))[[1L]],
    task
  )

  # PipeOp does not accept tasks with wrong feature types
  task = tsk("german_credit")
  expect_error(op$train(list(task)))

})
