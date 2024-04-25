context("PipeOpEncodeLmer")

sample_n_letters = function(n, l = 3) {
  factor(sample(head(letters, l), n, replace = TRUE))
}

test_that("PipeOpEncodeLmer regr", {
  skip_if_not_installed("nloptr")
  skip_if_not_installed("lme4")
  set.seed(8008135)
  task = mlr_tasks$get("boston_housing_classic")
  chaslevels = task$levels()$chas
  townlevels = task$levels()$town

  expect_datapreproc_pipeop_class(PipeOpEncodeLmer, task = task)
  expect_datapreproc_pipeop_class(PipeOpEncodeLmer, task = mlr_tasks$get("iris"))
  op = PipeOpEncodeLmer$new()
  expect_pipeop(op)
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  expect_true("factor" %nin% nt$feature_types$type)
  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  # factor cols are removed
  expect_true("factor" %nin% nt$feature_types$type)
})


test_that("PipeOpEncodeLmer multi and binaryclass", {
  skip_if_not_installed("nloptr")
  skip_if_not_installed("lme4")
  set.seed(8008135)

  # Multiclass
  task = mlr3::TaskClassif$new("task",
    data.table::data.table(x = sample_n_letters(20), y = sample_n_letters(20), z = seq_len(20)), "x")

  expect_datapreproc_pipeop_class(PipeOpEncodeLmer, task = task)
  op = PipeOpEncodeLmer$new()
  expect_pipeop(op)
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true("y" %nin% fn)
  expect_true("factor" %nin% nt$feature_types$type)
  # y is encoded
  expect_true(all(sprintf("y.%s", task$levels()$x) %in% fn))
  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  # factor cols are removed
  expect_true("y" %nin% fn)
  expect_true("factor" %nin% nt$feature_types$type)
  # y is encoded
  expect_true(all(sprintf("y.%s", task$levels()$x) %in% fn))

  # Binaryclass
  task = mlr3::TaskClassif$new("task",
    data.table::data.table(x = sample_n_letters(20, 2), y = sample_n_letters(20, 3), z = seq_len(20)), "x")

  expect_datapreproc_pipeop_class(PipeOpEncodeLmer, task = task)
  op = PipeOpEncodeLmer$new()
  expect_pipeop(op)
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  expect_true("factor" %nin% nt$feature_types$type)
  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  # factor cols are removed
  expect_true("factor" %nin% nt$feature_types$type)
})

test_that("PipeOpEncodeLmer Edge Cases", {
  skip_if_not_installed("nloptr")
  skip_if_not_installed("lme4")
  set.seed(8008135)
  task = mlr3::TaskClassif$new("task",
    data.table::data.table(x = sample_n_letters(10, 2), y = 1:10, z = 1:10), "x")

  expect_datapreproc_pipeop_class(PipeOpEncodeLmer, task = task)
  op = PipeOpEncodeLmer$new()
  expect_pipeop(op)
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true("factor" %nin% nt$feature_types$type)

  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  # factor cols are removed
  expect_true("factor" %nin% nt$feature_types$type)
})


test_that("Confirms to sensible values", {
  skip_if_not_installed("nloptr")
  skip_if_not_installed("lme4")
  logit = function(x) {exp(-x) / (1+exp(-x))}

  data = data.table::data.table(y = factor(sample_n_letters(200, 2)))
  data$x = factor(ifelse(data$y == "b", "b", "a"))
  task = mlr3::TaskClassif$new("task", data, "x")

  op = PipeOpEncodeLmer$new()
  expect_pipeop(op)

  nt = train_pipeop(op, inputs = list(task))[[1L]]
  yh = logit(nt$data()$y)
  expect_true(all(yh[data$x == "a"] > 0.9))
  expect_true(all(yh[data$x == "b"] < 0.1))
  expect_true("factor" %nin% nt$feature_types$type)

  nt = predict_pipeop(op, inputs = list(task))[[1L]]
  yh = logit(nt$data()$y)
  expect_true(all(yh[data$x == "a"] > 0.9))
  expect_true(all(yh[data$x == "b"] < 0.1))
  expect_true("factor" %nin% nt$feature_types$type)

  # Regression
  data = data.table::data.table(y = factor(sample_n_letters(200, 3)))
  data$x = ifelse(data$y == "a", -1, 1) + rnorm(200, 0, 10^-3)
  data$x = ifelse(data$y == "c", 0, data$x)
  task = mlr3::TaskRegr$new("task", data, "x")

  op = PipeOpEncodeLmer$new()
  expect_pipeop(op)

  nt = suppressWarnings(train_pipeop(op, inputs = list(task))[[1L]])  # suppress warnings about failures to converge
  yh = nt$data()$y
  expect_true(cor(data$x, yh) > 0.9)
  expect_true("factor" %nin% nt$feature_types$type)

  nt1 = predict_pipeop(op, inputs = list(task))[[1L]]
  yh = nt1$data()$y
  expect_true(cor(data$x, yh) > 0.9)
  expect_true("factor" %nin% nt1$feature_types$type)
  expect_equal(nt1, nt)
})
