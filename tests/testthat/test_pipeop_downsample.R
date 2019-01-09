context("downsample")

test_that("PipeOpDownsample - basic properties", {
  op = PipeOpDownsample$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))
})

test_that("PipeOpDownsample works unstratified", {
  task = mlr_tasks$get("iris")
  po = PipeOpDownsample$new()
  expect_class(po, "PipeOpDownsample")

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 0.7))

  pnew = predict_pipeop(po, list(task))
  expect_true(pnew[[1]]$nrow == task$nrow)

})

test_that("PipeOpDownsample works stratified", {
  task = mlr_tasks$get("iris")

  po = PipeOpDownsample$new()
  po$param_vals = list(stratify = TRUE, perc = 0.6)
  expect_class(po, "PipeOpDownsample")

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 0.6))
  # Proportions as expected
  expect_equal(table(tnew[[1]]$data(cols = tnew[[1]]$target_names)),
    table(rep(c("setosa", "versicolor", "virginica"), 30)))
})
