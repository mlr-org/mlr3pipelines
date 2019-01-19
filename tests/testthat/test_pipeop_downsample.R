context("PipeOpDownsample")

test_that("PipeOpDownsample - basic properties", {
  op = PipeOpDownsample$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))

  expect_datapreproc_pipeop_class(PipeOpDownsample, task = task,
    predict_like_train = FALSE, deterministic_train = FALSE)
})

test_that("PipeOpDownsample works unstratified", {
  task = mlr_tasks$get("iris")
  po = PipeOpDownsample$new()

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == task$nrow)

  po = PipeOpDownsample$new()
  po$param_vals$frac = 0.7

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 0.7))
  expect_subset(tnew[[1]]$row_roles$use, task$row_roles$use)

  pnew = predict_pipeop(po, list(task))
  expect_true(pnew[[1]]$nrow == task$nrow)
  expect_equal(pnew[[1]], task)

  task = mlr_tasks$get("iris")$filter(1L)  # actually has to be an int m(
  po = PipeOpDownsample$new()
  tnew = train_pipeop(po, list(task))

  task = mlr_tasks$get("bh")$filter(1L)  # actually has to be an int m(
  po = PipeOpDownsample$new()
  po$param_vals = list(stratify = TRUE, frac = 0.6)
  expect_error(train_pipeop(po, list(task)))

})

test_that("PipeOpDownsample works stratified", {
  task = mlr_tasks$get("iris")

  po = PipeOpDownsample$new()
  po$param_vals = list(stratify = TRUE, frac = 0.6)
  expect_class(po, "PipeOpDownsample")

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 0.6))
  # Proportions as expected
  expect_equal(table(tnew[[1]]$data(cols = tnew[[1]]$target_names)),
    table(rep(c("setosa", "versicolor", "virginica"), 30)))
})
