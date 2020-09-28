context("PipeOpHistBin")

test_that("PipeOpHistBin - basic properties", {
  task = mlr_tasks$get("iris")
  op = PipeOpHistBin$new()
  expect_pipeop(op)
  result = op$train(list(task))

  expect_datapreproc_pipeop_class(PipeOpHistBin, task = task)
  expect_task(result[[1L]])
  expect_equal(result[[1L]]$data(), op$predict(list(task))[[1L]]$data())

  a = apply(result[[1L]]$data()[, 2:5], MARGIN = 2L,
    function(x) expect_true(!anyMissing(x)))
})

test_that("PipeOpHistBin - change breaks", {
  task = mlr_tasks$get("iris")
  # 5 breaks --> 7 groups
  op5 = PipeOpHistBin$new(param_vals = list(breaks = 5L))
  expect_pipeop(op5)
  result = op5$train(list(task))
  b = apply(result[[1L]]$data(cols = result[[1L]]$feature_names), MARGIN = 2L,
    function(x) expect_lte(length(unique(x)), 7L)) # max 7
})

test_that("PipeOpHistBin - numerics out of range of training data", {
  task1 = mlr_tasks$get("iris")
  dat = iris
  dat$Sepal.Length[1L] = 2
  dat$Sepal.Width[1L] = 5
  task2 = TaskClassif$new("iris2", backend = dat, target = "Species")

  op = PipeOpHistBin$new()
  result1 = op$train(list(task1))
  ranges = sapply(op$state$bins, FUN = range)
  expect_equal(ranges[1L, ], rep.int(-Inf, times = 4L))
  expect_equal(ranges[2L, ], rep.int(Inf, times = 4L))

  result2 = op$predict(list(task2))
  c = apply(result2[[1L]]$data()[, 2:5], MARGIN = 2L,
    function(x) expect_true(!anyMissing(x)))

  bins = sapply(result2[[1L]]$data(), FUN = levels)[2:5]
  expect_true(all(sapply(bins, FUN = function(x) {
    as.logical(grep("-Inf", x[1])) && as.logical(grep("Inf", x[length(x)]))
  })))
})

test_that("PipeOpHistBin - not all bins present", {
  task1 = mlr_tasks$get("iris")
  dat = iris
  dat$Sepal.Width[[1L]] = 2.13
  task2 = TaskClassif$new("iris2", backend = dat, target = "Species")
  
  op = PipeOpHistBin$new(param_vals = list(breaks = seq(0, 10, by = 0.05)))
  expect_pipeop(op)
  
  # task1 does not have a Sepal.Width value within the interval (2.10, 2.15]
  bin_to_check = cut(c(2.10, 2.2), 2)[1] # (2.10, 2.15]
  
  result1 = op$train(list(task1))
  expect_false(bin_to_check %in% result1[[1L]]$data()$Sepal.Width)
  
  result2 = op$predict(list(task2))
  expect_true(bin_to_check %in% result2[[1L]]$data()$Sepal.Width)
  
  result3 = op$train(list(task2))
  expect_equal(result2[[1L]]$data(), result3[[1L]]$data())
})
