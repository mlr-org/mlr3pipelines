context("PipeOpCopy")


test_that("PipeOpCopy - basic properties", {
  expect_pipeop_class(PipeOpCopy, list(1))
  expect_pipeop_class(PipeOpCopy, list(3))
  expect_error(PipeOpCopy$new(0))

  po = PipeOpCopy$new(3)
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 1)
  expect_data_table(po$output, nrow = 3)
})


test_that("PipeOpCopy - train and predict", {
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
