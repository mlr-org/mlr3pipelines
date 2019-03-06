context("PipeOpImpute")

test_that("PipeOpImpute", {

  task = mlr_tasks$get("pima")

  expect_datapreproc_pipeop_class(PipeOpImpute, task = task)
