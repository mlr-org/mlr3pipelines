context("PipeOpSmote")

test_that("PipeOpSmote - basic properties", {
  skip_if_not_installed("smotefamily")
  set.seed(1234)
  data = smotefamily::sample_generator(1000, ratio = 0.80)
  data$result = as.factor(data$result)
  task = TaskClassif$new(id = "unbalanced", backend = data, target = "result")

  expect_datapreproc_pipeop_class(PipeOpSmote,
    task = task, predict_like_train = FALSE, deterministic_train = FALSE)

  op = PipeOpSmote$new(param_vals = list(K = 3))
  set.seed(1234)
  result = train_pipeop(op, inputs = list(task))

  expect_pipeop(op)
  expect_task(result[[1]])
})

test_that("compare to smotefamily::SMOTE", {
  skip_if_not_installed("smotefamily")
  set.seed(1234)
  data = smotefamily::sample_generator(1000, ratio = 0.80)
  data$result = as.factor(data$result)
  task = TaskClassif$new(id = "unbalanced", backend = data, target = "result")
  op = PipeOpSmote$new(param_vals = list(K = 3))
  set.seed(1234)
  result = train_pipeop(op, inputs = list(task))

  set.seed(1234)
  st = invoke(smotefamily::SMOTE, X = data[, -3], target = data[, 3], K = 3, .opts = list(warnPartialMatchArgs = FALSE))
  expect_equal(result[[1]]$data()[1001:nrow(st$data), c(2:3)], as.data.table(st$syn_data)[, 1:2])
  expect_equal(as.character(result[[1]]$data()[1001:nrow(st$data), result]), st$syn_data[, 3])
})
