library("testthat")

context("PipeOpInfo") # deprecated in 3rd edition

test_that("basic properties", {
  expect_pipeop_class(PipeOpInfo, list(id = "po"))
  po = PipeOpInfo$new("po")
  expect_pipeop(po)
})
