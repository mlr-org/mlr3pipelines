context("PipeOpFilterRows")

test_that("PipeOpFilterRows - basic properties", {
  op = PipeOpFilterRows$new()
  task = mlr_tasks$get("pima")
  expect_pipeop(op)
  expect_equal(train_pipeop(op, inputs = list(task))[[1L]], task)
  expect_equal(predict_pipeop(op, inputs = list(task))[[1L]], task)
  expect_datapreproc_pipeop_class(PipeOpFilterRows, task = task)
})

test_that("PipeOpFilterRows - filtering", {
  set.seed(1)
  task = tsk("pima")
  train_ids = sample(task$row_ids, size = 200)
  task_train = task$clone(deep = TRUE)$filter(train_ids)
  task_predict = task$clone(deep = TRUE)$filter(setdiff(task$row_ids, train_ids))
  dt_train = task_train$data(cols = task_train$feature_names)
  dt_predict = task_predict$data(cols = task_predict$feature_names)

  op = PipeOpFilterRows$new(param_vals = list(
    filter_formula = ~ (age < 31 & glucose > median(glucose, na.rm = TRUE)) |
      pedigree < mean(pedigree, na.rm = TRUE)))

  train_out = op$train(list(task_train))[[1L]]

  expect_equal(dt_train[(age < 31 & glucose > median(glucose, na.rm = TRUE)) |
    pedigree < mean(pedigree, na.rm = TRUE), ], train_out$data(cols = task_train$feature_names))

  predict_out = op$predict(list(task_predict))[[1L]]

  expect_equal(dt_predict[(age < 31 & glucose > median(glucose, na.rm = TRUE)) |
    pedigree < mean(pedigree, na.rm = TRUE), ], predict_out$data(cols = task_predict$feature_names))

  # Works with variables from an env
  env = new.env()
  assign("some_test_val", 7, envir = env)
  some_test_val = -100  # this should not be taken!
  filter_formula = ~ pregnant == some_test_val
  environment(filter_formula) = env
  op$param_set$values$filter_formula = filter_formula
  expect_true(all(op$train(list(task))[[1L]]$data(cols = "pregnant")[[1L]] == 7L))

  filter_formula = ~ pregnant == some_test_val & !apply(is.na(.SD), MARGIN = 1L, FUN = any)
  environment(filter_formula) = env
  op$param_set$values$filter_formula = filter_formula
  expect_equal(op$train(list(task))[[1L]]$data(), na.omit(task$data())[pregnant == 7, ])
})

test_that("PipeOpFilterRows - missing values removal", {
  set.seed(2)
  task = tsk("pima")
  train_ids = sample(task$row_ids, size = 200)
  task_train = task$clone(deep = TRUE)$filter(train_ids)
  task_predict = task$clone(deep = TRUE)$filter(setdiff(task$row_ids, train_ids))
  dt_train = task_train$data(cols = task_train$feature_names)
  dt_predict = task_predict$data(cols = task_predict$feature_names)

  op = PipeOpFilterRows$new(param_vals = list(filter_formula = ~ !is.na(insulin)))

  train_out = op$train(list(task_train))[[1L]]

  expect_equal(dt_train[!is.na(insulin), ],
    train_out$data(cols = task_train$feature_names))

  predict_out = op$predict(list(task_predict))[[1L]]

  expect_equal(dt_predict[!is.na(insulin), ],
    predict_out$data(cols = task_predict$feature_names))

  op$param_set$values$phase = "train"
  expect_equal(op$predict(list(task_predict))[[1L]], task_predict)

  op$param_set$values$phase = "predict"
  expect_equal(op$train(list(task_train))[[1L]], task_train)
})

test_that("PipeOpFilterRows - filtering and missing values removal", {
  set.seed(3)
  task = tsk("pima")
  train_ids = sample(task$row_ids, size = 200)
  task_train = task$clone(deep = TRUE)$filter(train_ids)
  task_predict = task$clone(deep = TRUE)$filter(setdiff(task$row_ids, train_ids))
  dt_train = task_train$data(cols = task_train$feature_names)
  dt_predict = task_predict$data(cols = task_predict$feature_names)

  op = PipeOpFilterRows$new(param_vals = list(
    filter_formula = ~ age > median(age, na.rm = TRUE) &
      !apply(is.na(.SD), MARGIN = 1L, FUN = any)))

  train_out = op$train(list(task_train))[[1L]]

  expect_equal(na.omit(dt_train[age > median(age, na.rm = TRUE)]),
    train_out$data(cols = task_train$feature_names))

  predict_out = op$predict(list(task_predict))[[1L]]

  expect_equal(na.omit(dt_predict[age > median(age, na.rm = TRUE)]),
    predict_out$data(cols = task_predict$feature_names))

  # Test with SDcols selector being explicitly set
  op$param_set$values$filter_formula = ~ !apply(is.na(.SD), MARGIN = 1L, FUN = any)
  op$param_set$values$SDcols = selector_name("insulin")
  expect_equal(op$train(list(task))[[1L]]$data(), task$data()[!is.na(insulin), ])
})

test_that("PipeOpFilterRows - check_filter_formulae", {
  expect_true(check_filter_formulae(NULL))
  expect_true(check_filter_formulae(~ age < 1))
  expect_character(check_filter_formulae(y ~ x))
})
