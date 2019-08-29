context("PipeOpHistBin")

task = mlr_tasks$get("iris")
op = PipeOpHistBin$new()
expect_pipeop(op)
result = op$train(list(task))

test_that("PipeOpHistBin - basic properties", {
  expect_datapreproc_pipeop_class(PipeOpHistBin, task = task)
  expect_task(result[[1]])
  expect_equal(result[[1]]$data(), op$predict(list(task))[[1]]$data())
})

test_that("PipeOpHistBin - see if expected result is returned", {
  # Default parameters
  a = apply(as.data.frame(result[[1]]$data()[, 2:5]), MARGIN = 2,
    function(x) expect_true(!anyMissing(x)))

  # 5 breaks --> 7 groups
  op5 = PipeOpHistBin$new(param_vals = list(breaks = 5))
  expect_pipeop(op5)
  result = op5$train(list(task))
  b = apply(as.data.frame(result[[1]]$data()[, 2:5]), MARGIN = 2,
    function(x) expect_true(length(unique(x)) <= 7)) # max 7
})
