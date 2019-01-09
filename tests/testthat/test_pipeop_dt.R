context("PipeOpDT")

test_that("PipeOpDT - basic properties", {
  po = PipeOpDT$new()
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 1)
  expect_data_table(po$output, nrow = 1)
})
