context("PipeOpUMAP")

test_that("PipeOpUMAP - basic properties", {
  op = PipeOpUMAP$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
})
