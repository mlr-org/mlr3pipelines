context("PipeOpScaleMaxAbs")

test_that("PipeOpScaleMaxAbs - basic properties", {
  task = mlr_tasks$get("iris")
  op = PipeOpScaleMaxAbs$new()
  expect_datapreproc_pipeop_class(PipeOpScaleMaxAbs, task = task)

  set.seed(1234)
  resdt = op$train(list(task))[[1]]$data()
  expect_true(all(sapply(resdt[, 2:5], max) == 1L))
  resmin = sapply(task$data()[, 2:5], min)/sapply(task$data()[, 2:5], max)
  expect_identical(sapply(resdt[, 2:5], min), resmin)
})

test_that("Other maxabs", {
  task = mlr_tasks$get("iris")

  op = PipeOpScaleMaxAbs$new(param_vals = list(maxabs = 0.6))
  set.seed(1234)
  resdt = op$train(list(task))[[1L]]$data()
  expect_true(all(sapply(resdt[, 2:5], max) == 0.6))
  resmin = (sapply(task$data()[, 2:5], min)/sapply(task$data()[, 2:5], max))*0.6
  expect_identical(sapply(resdt[, 2:5], min), resmin)
})
