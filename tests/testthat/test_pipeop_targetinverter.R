context("PipeOpTargetInverter")

test_that("PipeOpTargetInverter - basic properties", {
  expect_pipeop_class(PipeOpTargetInverter, list(id = "po"))

  po = PipeOpTargetInverter$new("po")

  expect_pipeop(po)
  expect_data_table(po$input, nrows = 2L)
  expect_data_table(po$output, nrows = 1L)
})
