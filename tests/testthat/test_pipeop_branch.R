context("PipeOpBranch")


test_that("PipeOpBranch - basic properties", {
  # Test basic properties
  op = PipeOpBranch$new(3)
  test_basic_pipeop_props(op)
  expect_true(length(op$train_intypes) == 1L)
  expect_true(length(op$predict_intypes) == 1L)
  expect_true(length(op$train_outtypes) == 3L)
  expect_true(length(op$predict_outtypes) == 3L)
})


test_that("PipeOpBranch - train and predict", {
  # Define PipeOp's
  branch = PipeOpBranch$new(2)
  t1 = mlr_tasks$get("iris")
  t2 = mlr_tasks$get("pima")


  branch = PipeOpBranch$new(2)
  tout = branch$train(list(t1))
  expect_true(branch$is_trained)
  expect_true(length(tout) == branch$outnum)
  expect_list(tout, len = branch$outnum)
  expect_class(tout[[branch$param_vals$selection]], "Task")
  expect_equal(tout[[branch$param_vals$selection]], t1)
  expect_class(tout[[2]], "NULL")

  branch2 = PipeOpBranch$new(2)
  branch2$param_vals$selection = 2L
  tout = branch2$train(list(t1))
  expect_true(branch2$is_trained)
  expect_true(length(tout) == branch2$outnum)
  expect_list(tout, len = branch2$outnum)
  expect_class(tout[[branch2$param_vals$selection]], "Task")
  expect_equal(tout[[branch2$param_vals$selection]], t1)
  expect_equal(tout[[1]], NULL)
  
  pout = branch$predict(list(task))
  expect_true(length(pout) == branch$outnum)
  expect_equal(pout[[1]], t1)
  expect_equal(pout[[2]], NULL)
})