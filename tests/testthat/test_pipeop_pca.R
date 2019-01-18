context("PipeOpPCA")

test_that("PipeOpPCA - basic properties", {
  op = PipeOpPCA$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)

  expect_datapreproc_pipeop_class(PipeOpPCA, task = task)

  # FIXME: either we dont need this, as this is already ensured in PipeOp, or we should test this in expect_pipeop
  result = train_pipeop(op, inputs = list(task))
  expect_task(result[[1]])

  result = predict_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
})


test_that("PipeOpScale - basic properties", {
  op = PipeOpScale$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)

  expect_datapreproc_pipeop_class(PipeOpScale, task = task)

  result = train_pipeop(op, inputs = list(task))
  expect_task(result[[1]])

  result = predict_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
})
