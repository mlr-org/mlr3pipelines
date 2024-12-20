context("PipeOpEncodePL")

test_that("PipeOpEncodePL - basic properties", {
  task = mlr_tasks$get("mtcars")
  expect_datapreproc_pipeop_class(PipeOpEncodePL, constargs = list(task_type = "TaskRegr"), task = task)

  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpEncodePL, task = task)
  expect_datapreproc_pipeop_class(PipeOpEncodePL, constargs = list(task_type = "TaskClassif"), task = task)
})

# Tests:
# - different methods
#    - with params (not all for regtree, hopefully)
# - test on tasks with simple data that behaviour is as expected (compare dts)
# - TODO: decide how to handle NAs in feature columns and test that
