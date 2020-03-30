context("PipeOpHistBin")

test_that("PipeOpHistBin - basic properties", {
  task = mlr_tasks$get("iris")
  op = PipeOpHistBin$new()
  expect_pipeop(op)
  result = op$train(list(task))

  expect_datapreproc_pipeop_class(PipeOpHistBin, task = task)
  expect_task(result[[1]])
  expect_equal(result[[1]]$data(), op$predict(list(task))[[1]]$data())

  a = apply(as.data.frame(result[[1]]$data()[, 2:5]), MARGIN = 2,
    function(x) expect_true(!anyMissing(x)))
})

test_that("PipeOpHistBin - change breaks", {
  task = mlr_tasks$get("iris")
  # 5 breaks --> 7 groups
  op5 = PipeOpHistBin$new(param_vals = list(breaks = 5))
  expect_pipeop(op5)
  result = op5$train(list(task))
  b = apply(result[[1]]$data(cols = result[[1]]$feature_names), MARGIN = 2,
    function(x) expect_lte(length(unique(x)), 7)) # max 7
})
