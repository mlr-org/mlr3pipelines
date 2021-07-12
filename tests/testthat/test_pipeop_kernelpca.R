context("PipeOpKernelPCA")


test_that("PipeOpKernelPCA - basic properties", {
  skip_if_not_installed("kernlab")

  task = mlr_tasks$get("iris")

  expect_datapreproc_pipeop_class(PipeOpKernelPCA, task = task,
    deterministic_train = TRUE, deterministic_predict = TRUE, tolerance = 1e-4)

  op = PipeOpKernelPCA$new()
  expect_pipeop(op)
  set.seed(1234)
  result = op$train(list(task))

  expect_task(result[[1]])
  expect_equal(result[[1]]$data(), op$predict(list(task))[[1]]$data())
})

test_that("PipeOpKernelPCA - compare to kernlab::kpca", {
  skip_if_not_installed("kernlab")

  task = mlr_tasks$get("iris")
  op = PipeOpKernelPCA$new()
  expect_pipeop(op)
  set.seed(1234)
  result = op$train(list(task))

  # Default parameters
  dt = task$data()[, 2:5]
  set.seed(1234)
  pca = kernlab::kpca(as.matrix(dt))
  expect_equal(dim(result[[1]]$data()[, -1]), dim(kernlab::rotated(pca)))
  expect_identical(result[[1]]$data()[, -1], as.data.table(kernlab::rotated(pca)))

  # Change some parameters
  op2 = PipeOpKernelPCA$new(param_vals = list(kpar = list(sigma = 0.4), features = 4))
  expect_pipeop(op2)
  set.seed(1234)
  result2 = op2$train(list(task))
  set.seed(1234)
  pca2 = kernlab::kpca(as.matrix(dt), kpar = list(sigma = 0.4), features = 4)
  expect_true(all.equal(dim(kernlab::rotated(pca2)), dim(result2[[1]]$data()[, -1])))
  dtres = as.matrix(result2[[1]]$data()[, -1])
  dimnames(dtres) = NULL
  expect_equal(dtres, kernlab::rotated(pca2))
})
