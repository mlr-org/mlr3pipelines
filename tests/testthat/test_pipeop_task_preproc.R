context("PipeOpTaskPreproc")

test_that("PipeOpTaskPreproc - basic properties", {
  expect_pipeop_class(PipeOpTaskPreproc, list(id = "potask"))

  po = PipeOpTaskPreproc$new("potask")

  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)
})

test_that("PipeOpTaskPreprocSimple - basic properties", {
  expect_pipeop_class(PipeOpTaskPreprocSimple, list(id = "posimple"))

  po = PipeOpTaskPreprocSimple$new(id = "posimple")
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)
})
