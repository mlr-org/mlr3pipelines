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

test_that("Wrong affect_columns errors", {
  POPP = R6Class("POPP",
    inherit = PipeOpTaskPreproc,
    public = list(
      train_dt = function(dt, levels, target) dt,
      predict_dt = function(dt, levels) dt
    )
  )
  tsk = tsk("boston_housing_classic")
  po = POPP$new("foo", param_vals = list(affect_columns = is.factor))
  expect_pipeop(po)
  expect_error(po$train(list(tsk)), "affected_cols")

  po = POPP$new("foo", param_vals = list(affect_columns = function(x) x$target_names))
  expect_error(po$train(list(tsk)), "affected_cols")
})
