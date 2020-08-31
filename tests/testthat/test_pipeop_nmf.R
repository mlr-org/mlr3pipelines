context("PipeOpNMF")

test_that("PipeOpNMF - basic properties", {
  op = PipeOpNMF$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)

  expect_datapreproc_pipeop_class(PipeOpNMF, task = task, deterministic_train = FALSE)
})

test_that("PipeOpNMF - feature selector", {
  op = PipeOpNMF$new()
  dat = iris
  dat$Sepal.Length[1L] <- -999
  dat$test <- rep.int(c(TRUE, FALSE), times = 75L)
  task = TaskClassif$new("test", backend = dat, target = "Species")
  train_out = op$train(list(task))[[1L]]
  expect_setequal(c("Sepal.Length", "test", paste0("NMF", 1:2)), train_out$feature_names)
})


test_that("PipeOpNMF - parameters", {
  op = PipeOpNMF$new()
  op$param_set$values$rank = 3L
  op$param_set$values$nrun = 2L
  op$param_set$values$seed = 999

  task = mlr_tasks$get("iris")

  train_out1 = op$train(list(task))[[1L]]
  expect_subset(paste0("NMF", 1:3), train_out1$feature_names)
  expect_equal(op$state@nrun, 2L)
  expect_matrix(op$state@consensus)

  train_out2 = op$train(list(task))[[1L]]
  expect_equal(train_out1, train_out2)
})
