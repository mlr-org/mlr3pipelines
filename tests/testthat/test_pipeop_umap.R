context("PipeOpUMAP")

test_that("PipeOpUMAP - basic properties", {
  skip_if_not_installed("uwot")
  op = PipeOpUMAP$new()
  expect_pipeop(op)
})
