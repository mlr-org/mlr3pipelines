context("PipeOpQuantileBin")

test_that("PipeOpQuantileBin - basic properties", {
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpQuantileBin, task = task)
})

test_that("PipeOpQuantileBin - see if expected result is returned", {
  task = mlr_tasks$get("iris")
  op = PipeOpQuantileBin$new()
  expect_pipeop(op)
  result = op$train(list(task))

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
