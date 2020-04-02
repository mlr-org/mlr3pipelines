context("PipeOpTargetTrafo")

test_that("PipeOpTargetTrafo - basic properties", {
  expect_pipeop_class(PipeOpTargetTrafo, list(id = "po"))

  po = PipeOpTargetTrafo$new("po")

  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 2)
})
