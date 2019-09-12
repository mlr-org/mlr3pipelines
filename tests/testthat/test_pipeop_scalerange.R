context("PipeOpScaleRange")

test_that("PipeOpScaleRange - basic properties", {
  task = mlr_tasks$get("iris")
  op = PipeOpScaleRange$new()

  expect_pipeop(op)
  expect_datapreproc_pipeop_class(PipeOpScaleRange, task = task)

  set.seed(1234)
  result = op$train(list(task))
  resdt = result[[1]]$data()

  expect_task(result[[1]])
  expect_equal(resdt, op$predict(list(task))[[1]]$data())
  expect_true(all(sapply(resdt[, 2:5], max) == 1L))
  expect_true(all(sapply(resdt[, 2:5], min) == 0L))
}
)

test_that("Other maxabs", {
  op = PipeOpScaleRange$new(param_vals = list(upper = 0.6, lower = 0.2))
  set.seed(1234)
  result = op$train(list(task))
  resdt = result[[1]]$data()
  resdt.max = sapply(resdt[, 2:5], max)
  names(resdt.max) = NULL
  expect_equal(resdt.max, rep(0.6, 4))
  resdt.min = sapply(resdt[, 2:5], min)
  names(resdt.min) = NULL
  expect_equal(resdt.min, rep(0.2, 4))
})

