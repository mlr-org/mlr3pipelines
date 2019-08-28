context("PipeOpQuantileBin")

task = mlr_tasks$get("iris")
op = PipeOpQuantileBin$new()
expect_pipeop(op)
result = op$train(list(task))

test_that("PipeOpQuantileBin - basic properties", {
  expect_datapreproc_pipeop_class(PipeOpQuantileBin, task = task)
  expect_task(result[[1]])
  expect_equal(result[[1]]$data(), op$predict(list(task))[[1]]$data())
})

test_that("PipeOpQuantileBin - see if expected result is returned", {
  # Default parameters
  a = apply(as.data.frame(result[[1]]$data()[, 2:5]), MARGIN = 2,
    function(x) expect_equal(length(unique(x)), 2))

  # 5 bins
  op5 = PipeOpQuantileBin$new(param_vals = list(numsplits = 5))
  expect_pipeop(op5)
  result = op5$train(list(task))
  b = apply(as.data.frame(result[[1]]$data()[, 2:5]), MARGIN = 2,
    function(x) expect_equal(length(unique(x)), 5))
})
