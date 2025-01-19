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
    private = list(
      .train_dt = function(dt, levels, target) dt,
      .predict_dt = function(dt, levels) dt
    )
  )
  tsk = tsk("boston_housing_classic")
  po = POPP$new("foo", param_vals = list(affect_columns = is.factor))
  expect_pipeop(po)
  expect_error(po$train(list(tsk)), "affected_cols")

  po = POPP$new("foo", param_vals = list(affect_columns = function(x) x$target_names))
  expect_error(po$train(list(tsk)), "affected_cols")
})

test_that("PipeOpTaskPreproc - fix for #864 works", {
  # Fixes #864: A column that is a feature and something else does not loose the other role during training or prediction
  POPP = R6Class("POPP",
    inherit = PipeOpTaskPreproc,
    private = list(
      .train_dt = function(dt, levels, target) dt,
      .predict_dt = function(dt, levels) dt
    )
  )
  po = POPP$new("test", param_vals = list(affect_columns = selector_name("Petal.Length")))
  expect_pipeop(po)
  task = tsk("iris")
  task$col_roles$order = "Petal.Width"

  train_out = po$train(list(task))[[1L]]
  expect_equal(train_out$col_roles, task$col_roles)

  predict_out = po$predict(list(task))[[1L]]
  expect_equal(predict_out$col_roles, task$col_roles)
})
