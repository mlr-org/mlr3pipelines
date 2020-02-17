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

test_that("Error + message", {
  POPP = R6Class("POPP",
    inherit = PipeOpTaskPreproc,
    public = list(
      train_dt = function(dt, levels, target) dt,
      predict_dt = function(dt, levels) dt
    )
  )
  po = POPP$new("foo", param_vals = list(affect_columns = is.factor))
  expect_pipeop(po)
  tsk = tsk("boston_housing")
  expect_error(po$train(list(tsk)))
})
