context("PipeOpMaterialize")

make_materialize_task = function(rows = as.integer(c(1, 3, 5, 7))) {
  task = tsk("iris")
  task$cbind(data.table(extra = seq_len(task$nrow), unused = seq_len(task$nrow) + 1000L))
  task$select(c("Petal.Length", "extra"))
  task$filter(rows)
  task
}

test_that("PipeOpMaterialize - basic properties", {
  expect_pipeop_class(PipeOpMaterialize)

  op = po("materialize")
  task = tsk("iris")
  train_pipeop(op, list(task))
  predict_pipeop(op, list(task))
})

test_that("PipeOpMaterialize - active task view materialization works", {
  intask = make_materialize_task()
  input_data = intask$data()
  input_backend_nrow = intask$backend$nrow
  input_backend_cols = intask$backend$colnames
  expected_backend_cols = c(intask$backend$primary_key, intask$target_names, intask$feature_names)
  dropped_cols = c("Sepal.Length", "Sepal.Width", "Petal.Width", "unused")

  op = po("materialize")
  train_out = op$train(list(intask))[[1L]]

  # task was not modified in-place
  expect_equal(intask$data(), input_data)
  expect_equal(intask$backend$nrow, input_backend_nrow)
  expect_set_equal(intask$backend$colnames, input_backend_cols)

  # train worked
  expect_true(op$is_trained)
  expect_equal(op$state, list())
  # task view is unchanged
  expect_equal(train_out$data(), input_data)
  expect_equal(train_out$row_ids, intask$row_ids)
  expect_set_equal(train_out$feature_names, c("Petal.Length", "extra"))
  # backend was materialized
  expect_equal(train_out$backend$nrow, length(unique(intask$row_ids)))
  expect_set_equal(train_out$backend$colnames, expected_backend_cols)
  expect_false(any(dropped_cols %in% train_out$backend$colnames))

  predict_out = op$predict(list(intask))[[1L]]

  # task was not modified in-place
  expect_equal(intask$data(), input_data)
  expect_equal(intask$backend$nrow, input_backend_nrow)
  expect_set_equal(intask$backend$colnames, input_backend_cols)

  # task view is unchanged
  expect_equal(predict_out$data(), input_data)
  expect_equal(predict_out$row_ids, intask$row_ids)
  expect_set_equal(predict_out$feature_names, c("Petal.Length", "extra"))
  # backend was materialized
  expect_equal(predict_out$backend$nrow, length(unique(intask$row_ids)))
  expect_set_equal(predict_out$backend$colnames, expected_backend_cols)
  expect_false(any(dropped_cols %in% predict_out$backend$colnames))
})

test_that("PipeOpMaterialize - internal validation task is only materialized during training", {
  task = make_materialize_task(as.integer(c(11, 13, 15)))
  validation_task = make_materialize_task(as.integer(c(1, 3, 5, 7)))
  task$internal_valid_task = validation_task
  validation_data = validation_task$data()
  validation_backend_nrow = validation_task$backend$nrow
  validation_backend_cols = validation_task$backend$colnames
  expected_validation_backend_cols = c(validation_task$backend$primary_key,
    validation_task$target_names, validation_task$feature_names)
  dropped_cols = c("Sepal.Length", "Sepal.Width", "Petal.Width", "unused")

  op = po("materialize")
  train_out = train_pipeop(op, list(task))[[1L]]
  predict_out = predict_pipeop(op, list(task))[[1L]]

  # train materializes the internal validation task
  expect_equal(train_out$internal_valid_task$backend$nrow, length(unique(validation_task$row_ids)))
  expect_set_equal(train_out$internal_valid_task$backend$colnames, expected_validation_backend_cols)
  expect_false(any(dropped_cols %in% train_out$internal_valid_task$backend$colnames))

  # predict leaves the internal validation task unchanged
  expect_equal(predict_out$internal_valid_task$backend$nrow, validation_backend_nrow)
  expect_set_equal(predict_out$internal_valid_task$backend$colnames, validation_backend_cols)
})

test_that("PipeOpMaterialize - duplicate row ids remain active but are stored once", {
  row_ids = as.integer(c(1, 2, 1, 3, 2))
  task = tsk("iris")
  task$row_roles$use = row_ids

  op = po("materialize")
  train_out = op$train(list(task))[[1L]]
  predict_out = op$predict(list(task))[[1L]]

  expect_equal(train_out$row_ids, row_ids)
  expect_equal(train_out$backend$nrow, length(unique(row_ids)))

  expect_equal(predict_out$row_ids, row_ids)
  expect_equal(predict_out$backend$nrow, length(unique(row_ids)))
})