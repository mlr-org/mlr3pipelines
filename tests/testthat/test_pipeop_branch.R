context("PipeOpBranch")

test_that("PipeOpBranch - basic properties", {
  # Test basic properties
  op = PipeOpBranch$new(3)
  expect_pipeop(op)
  # expect_true(length(op$train_intypes) == 1L)
  # expect_true(length(op$predict_intypes) == 1L)
  # expect_true(length(op$train_outtypes) == 3L)
  # expect_true(length(op$predict_outtypes) == 3L)
})


test_that("PipeOpBranch - train and predict", {
  # Define PipeOp's
  t1 = mlr_tasks$get("iris")

  branch = PipeOpBranch$new(2)
  tout = train_pipeop(branch, list(t1))
  expect_true(length(tout) == branch$outnum)
  expect_list(tout, len = branch$outnum)
  expect_class(tout[[branch$param_vals$selection]], "Task")
  expect_equal(tout[[branch$param_vals$selection]], t1)
  expect_class(tout[[2]], "NULL")

  branch2 = PipeOpBranch$new(2)
  branch2$param_vals$selection = 2L
  tout = train_pipeop(branch2, list(t1))
  # expect_true(length(tout) == branch2$outnum)
  # expect_list(tout, len = branch2$outnum)
  expect_class(tout[[branch2$param_vals$selection]], "Task")
  expect_equal(tout[[branch2$param_vals$selection]], t1)
  expect_equal(tout[[1]], NULL)

  pout = predict_pipeop(branch, list(t1))
  # expect_true(length(pout) == branch$outnum)
  expect_equal(pout[[1]], t1)
  expect_equal(pout[[2]], NULL)
})
