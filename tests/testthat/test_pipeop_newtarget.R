test_that("convert_task reconstructs task", {
  task = tsk("iris")
  tsk = convert_task(task)
  tsk$man = "mlr3::mlr_tasks_iris"
  expect_equal(task, tsk, check.attributes = FALSE)

  task2 = task$filter(1:100)
  tsk2 = convert_task(task2)
  expect_equal(task2$nrow, tsk2$nrow)
  expect_equal(task2$ncol, tsk2$ncol)
  expect_true("twoclass" %in% tsk2$properties)

  task3 = task2$set_row_role(101:150, "use")
  tsk3 = convert_task(task3)
  tsk3$man = "mlr3::mlr_tasks_iris"
  expect_equal(task3$nrow, tsk3$nrow)
  expect_equal(task3$ncol, tsk3$ncol)
  expect_true("multiclass" %in% tsk3$properties)
  expect_equal(task, tsk3, check.attributes = FALSE)
})
