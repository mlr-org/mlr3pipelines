context("PipeOpTomek")

test_that("PipeOpTomek - basic properties", {
  skip_if_not_installed("themis")

  task = mlr_tasks$get("iris")

  expect_datapreproc_pipeop_class(PipeOpTomek, task = task, predict_like_train = FALSE)

})

test_that("PipeOpTomek - train works as intended", {
  skip_if_not_installed("themis")

  op = PipeOpTomek$new()
  task = mlr_tasks$get("iris")

  # Compare to themis::tomek
  train_out = op$train(list(task))[[1]]$data()
  smotenc_out = setDT(invoke(themis::tomek, df = task$data(), var = task$target_names))

  expect_equal(train_out, smotenc_out)

  # Empty task is returned unchanged
  task$select(character(0))
  expect_equal(
    op$train(list(task))[[1L]],
    task
  )

  # PipeOp does not accept tasks with wrong feature types
  task = tsk("breast_cancer")
  expect_error(op$train(list(task)))

})
