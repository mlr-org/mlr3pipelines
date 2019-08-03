context("PipeOpUnbranch")


test_that("PipeOpUnbranch - basic properties", {
  po = PipeOpUnbranch$new(3)
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 3)
  expect_data_table(po$output, nrow = 1)

  expect_pipeop_class(PipeOpUnbranch, list(1))
  expect_pipeop_class(PipeOpUnbranch, list(3))

  po = PipeOpUnbranch$new()
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 1)
})


test_that("PipeOpUnbranch - train and predict", {
  t1 = mlr_tasks$get("iris")
  t2 = mlr_tasks$get("pima")

  ubranch = PipeOpUnbranch$new(2)
  expect_true(ubranch$innum == 2L)

  tout = train_pipeop(ubranch, (list(t1, NO_OP)))
  expect_class(tout[[1]], "Task")
  expect_true(length(tout) == 1L)

  pout = predict_pipeop(ubranch, (list(NO_OP, t2)))
  expect_true(length(pout) == 1)
  expect_equal(pout[[1]], t2)

  expect_error(ubranch$train(list(t1, t2)))
  expect_error(ubranch$train(list(t1)))

  ubranch = PipeOpUnbranch$new()
  expect_true(ubranch$innum == 1)

  tout = train_pipeop(ubranch, (list(t1, NO_OP)))
  expect_class(tout[[1]], "Task")
  expect_true(length(tout) == 1L)

  pout = predict_pipeop(ubranch, (list(NO_OP, t2)))
  expect_true(length(pout) == 1)
  expect_equal(pout[[1]], t2)

  ubranch = PipeOpUnbranch$new()
  tout = train_pipeop(ubranch, (list(t1)))
  expect_class(tout[[1]], "Task")
  expect_true(length(tout) == 1L)

  pout = predict_pipeop(ubranch, (list(t2)))
  expect_true(length(pout) == 1)
  expect_equal(pout[[1]], t2)

})
