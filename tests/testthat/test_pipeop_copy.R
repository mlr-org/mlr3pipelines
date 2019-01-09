context("PipeOpCopy")


test_that("PipeOpCopy - basic properties", {
  # Test basic properties
  op = PipeOpCopy$new(3)
  expect_pipeop(op)
  expect_true(length(op$train_intypes) == 1L)
  expect_true(length(op$predict_intypes) == 1L)
  expect_true(length(op$train_outtypes) == 3L)
  expect_true(length(op$predict_outtypes) == 3L)
})


test_that("PipeOpCopy - train and predict", {
  # Define PipeOp's
  copy = PipeOpCopy$new(2)
  task = mlr_tasks$get("iris")

  tout = train_pipeop(copy, list(task))
  expect_true(length(tout) == copy$outnum)
  expect_list(tout, types = "Task")
  expect_equal(tout[[1]], task)
  expect_equal(tout[[2]], task)

  pout = predict_pipeop(copy, list(task))
  expect_true(length(tout) == copy$outnum)
  expect_list(pout, types = "Task")
  expect_equal(pout[[1]], task)
  expect_equal(pout[[2]], task)
})
