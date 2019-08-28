context("PipeOpChunk")

test_that("PipeOpChunk - basic properties", {
  expect_pipeop_class(PipeOpChunk, list(1))
  expect_pipeop_class(PipeOpChunk, list(3))
  expect_error(PipeOpChunk$new(0))

  po = PipeOpChunk$new(3)
  expect_pipeop(po)

  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 3)
})
