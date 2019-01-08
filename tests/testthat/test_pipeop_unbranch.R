context("PipeOpUnbranch")


test_that("PipeOpUnbranch - basic properties", {
  # Test basic properties
  op = PipeOpUnbranch$new(3)
  test_basic_pipeop_props(op)
  expect_true(length(op$train_intypes) == 3L)
  expect_true(length(op$predict_intypes) == 3L)
  expect_true(length(op$train_outtypes) == 1L)
  expect_true(length(op$predict_outtypes) == 1L)
  expect_true(op$innum == 3)
})


test_that("PipeOpUnbranch - train and predict", {
  # Define PipeOp's
  t1 = mlr_tasks$get("iris")
  t2 = mlr_tasks$get("pima")

  ubranch = PipeOpUnbranch$new(2)
  expect_true(ubranch$innum == 2L)
  tout = ubranch$train(list(t1, NULL)) 
  expect_true(ubranch$is_trained)
  expect_class(tout[[1]], "Task")
  expect_true(length(tout) == 1L)

  expect_error(ubranch$train(list(t1)))

  pout = ubranch$predict(list(NULL, t2))
  expect_true(length(pout) == 1)
  expect_equal(pout[[1]], t2)
})