context("PipeOpSubsample")

test_that("PipeOpSubsample - basic properties", {
  op = PipeOpSubsample$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))

  expect_datapreproc_pipeop_class(PipeOpSubsample, task = task,
    predict_like_train = FALSE, deterministic_train = FALSE)
})

test_that("PipeOpSubsample works unstratified", {
  task = mlr_tasks$get("iris")
  po = PipeOpSubsample$new()

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == task$nrow)

  po = PipeOpSubsample$new()
  po$param_set$values$frac = 0.7

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 0.7))
  expect_subset(tnew[[1]]$row_roles$use, task$row_roles$use)

  pnew = predict_pipeop(po, list(task))
  expect_true(pnew[[1]]$nrow == task$nrow)
  expect_equal(pnew[[1]], task)

  task = mlr_tasks$get("iris")$filter(1L)  # actually has to be an int m(
  po = PipeOpSubsample$new()
  tnew = train_pipeop(po, list(task))

  task = mlr_tasks$get("bh")$filter(1L)  # actually has to be an int m(
  po = PipeOpSubsample$new()
  po$param_set$values = list(stratify = TRUE, frac = 0.6)
  expect_error(train_pipeop(po, list(task)))

})

test_that("PipeOpSubsample works stratified", {
  task = mlr_tasks$get("iris")

  po = PipeOpSubsample$new()
  po$param_set$values = list(stratify = TRUE, frac = 0.6, replace = FALSE)
  expect_class(po, "PipeOpSubsample")

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 0.6))
  # Proportions as expected
  expect_equal(table(tnew[[1]]$data(cols = tnew[[1]]$target_names)),
    table(rep(c("setosa", "versicolor", "virginica"), 30)))

  po = PipeOpSubsample$new()
  po$param_set$values = list(stratify = TRUE, frac = 0.6, replace = TRUE)
  expect_class(po, "PipeOpSubsample")

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 0.6))
  # Proportions as expected
  expect_equal(table(tnew[[1]]$data(cols = tnew[[1]]$target_names)),
    table(rep(c("setosa", "versicolor", "virginica"), 30)))

  po = PipeOpSubsample$new()
  po$param_set$values = list(stratify = TRUE, frac = 2, replace = TRUE)
  expect_class(po, "PipeOpSubsample")

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 2))
  # Proportions as expected
  expect_equal(table(tnew[[1]]$data(cols = tnew[[1]]$target_names)),
    table(rep(c("setosa", "versicolor", "virginica"), 100)))

})



test_that("task filter utility function", {
  task = mlr_tasks$get("iris")

  rowidx = c(1, 2, 3, 2, 1, 2, 3, 2, 1)

  tfiltered = task_filter_ex(task$clone(), rowidx)

  expect_equal(tfiltered$data(), task$data(rows = rowidx))

  task$select(c("Petal.Length", "Petal.Width"))

  tfiltered = task_filter_ex(task$clone(), rowidx)

  expect_equal(tfiltered$data(), task$data(rows = rowidx))

})
