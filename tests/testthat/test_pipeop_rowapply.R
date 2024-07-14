context("PipeOpRowApply")

test_that("apply general tests", {

  op = PipeOpRowApply$new()
  expect_pipeop(op)

  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpRowApply, task = task,
                                  constargs = list(param_vals = list(applicator = as.integer)))

  expect_datapreproc_pipeop_class(PipeOpRolApply, task = mlr_tasks$get("pima"),
                                  constargs = list(param_vals = list(applicator = as.numeric)))

})


