context("PipeOpTargetInvert")

test_that("PipeOpTargetInvert - basic properties", {
  expect_pipeop_class(PipeOpTargetInvert, list(id = "po"))

  po = PipeOpTargetInvert$new("po")

  expect_pipeop(po)
  expect_data_table(po$input, nrows = 2L)
  expect_data_table(po$output, nrows = 1L)
})
