context("PipeOpKernelPCA")

require(kernlab)
task = mlr_tasks$get("iris")
op = PipeOpKernelPCA$new()
expect_pipeop(op)
set.seed(1234)
result = op$train(list(task))

test_that("PipeOpKernelPCA - basic properties", {
  expect_datapreproc_pipeop_class(PipeOpKernelPCA, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE)
  expect_task(result[[1]])
  expect_equal(result[[1]]$data(), op$predict(list(task))[[1]]$data())
})

test_that("PipeOpKernelPCA - compare to kernlab::kpca", {
  # Default parameters
  dt = task$data()[, 2:5]
  set.seed(1234)
  pca = kpca(as.matrix(dt))
  expect_equal(dim(result[[1]]$data()[, -1]), dim(rotated(pca)))
  expect_identical(result[[1]]$data()[, -1], as.data.table(rotated(pca)))

  # Change some parameters
  op2 = PipeOpKernelPCA$new(param_vals = list(kpar = list(sigma = 0.4), features = 4))
  expect_pipeop(op2)
  set.seed(1234)
  result2 = op2$train(list(task))
  set.seed(1234)
  pca2 = kpca(as.matrix(dt), kpar = list(sigma = 0.4), features = 4)
  expect_true(all.equal(dim(rotated(pca2)), dim(result2[[1]]$data()[, -1])))
  dtres = as.matrix(result2[[1]]$data()[, -1])
  dimnames(dtres) = NULL
  expect_equal(dtres, rotated(pca2))
})
