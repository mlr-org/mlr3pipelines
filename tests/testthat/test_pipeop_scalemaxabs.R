context("PipeOpScaleMaxAbs")

task = mlr_tasks$get("iris")
op = PipeOpScaleMaxAbs$new()
set.seed(1234)
result = op$train(list(task))
resdt = result[[1]]$data()

test_that("PipeOpScaleMaxAbs - basic properties", {
  expect_pipeop(op)
  expect_datapreproc_pipeop_class(PipeOpScaleMaxAbs, task = task)
  expect_task(result[[1]])
  expect_equal(resdt, op$predict(list(task))[[1]]$data())
  expect_true(all(sapply(resdt[, 2:5], max) == 1L))
  resmin = sapply(task$data()[, 2:5], min)/sapply(task$data()[, 2:5], max)
  expect_identical(sapply(resdt[, 2:5], min), resmin)
}
)

test_that("Other maxabs", {
  op = PipeOpScaleMaxAbs$new(param_vals = list(maxabs = 0.6))
  set.seed(1234)
  result = op$train(list(task))
  resdt = result[[1]]$data()
  expect_true(all(sapply(resdt[, 2:5], max) == 0.6))
  resmin = (sapply(task$data()[, 2:5], min)/sapply(task$data()[, 2:5], max))*0.6
  expect_identical(sapply(resdt[, 2:5], min), resmin)
})
