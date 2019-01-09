context("PipeOpChunk")

test_that("PipeOpChunk - basic properties", {
  po = PipeOpChunk$new(3)
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 1)
  expect_data_table(po$output, nrow = 3)
})
