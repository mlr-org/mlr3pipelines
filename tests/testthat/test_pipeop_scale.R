context("PipeOpScale")

test_that("PipeOpScale - basic properties", {
  op = PipeOpScale$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)

  result = train_pipeop(op, inputs = list(task))
  expect_task(result[[1]])

  result = predict_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
})
