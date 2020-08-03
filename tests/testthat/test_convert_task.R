test_that("convert_task - Regr -> Regr", {
  check_result_regr = function(result) {
    expect_class(result, "TaskRegr")
    expect_task(result)
    expect_true(result$col_roles$target == "age")
    expect_true(all(result$feature_names != "age"))
    expect_true(all(result$feature_names != "medv"))
    expect_true(all(unlist(imap(result$row_roles,
      .f = function(z, x) {all(result$row_roles[[x]] == task$row_roles[[x]])}))
    ))
    expect_true(
      all(map_lgl(c("weights", "groups", "strata", "nrow"), function(x) {
        all(result[[x]] == task[[x]])
    })))
  }
  task = mlr_tasks$get("boston_housing")
  check_result_regr(convert_task(task, new_target = "age", drop_original_target = TRUE))
})

test_that("convert_task - Regr -> Classif", {
  check_result_classif = function(result) {
    expect_class(result, "TaskClassif")
    expect_task(result)
    expect_true(result$col_roles$target == "chas")
    expect_true(all(result$feature_names != "chas"))
    expect_true(all(result$feature_names != "medv"))
    expect_true(all(unlist(imap(result$row_roles,
      .f = function(z, x) {all(result$row_roles[[x]] == task$row_roles[[x]])}))
    ))
    expect_true(
      all(map_lgl(c("weights", "groups", "strata", "nrow"), function(x) {
        all(result[[x]] == task[[x]])
    })))
  }
  task = mlr_tasks$get("boston_housing")
  check_result_classif(convert_task(task, new_target = "chas", new_type = "classif", drop_original_target = TRUE))
})

test_that("convert_task - Classif -> Regr", {
  check_result_regr = function(result) {
    expect_class(result, "TaskRegr")
    expect_task(result)
    expect_true(result$col_roles$target == "Sepal.Width")
    expect_true(all(result$feature_names != "Sepal.Width"))
    expect_true(all(result$feature_names != "Species"))
    expect_true(all(unlist(imap(result$row_roles,
      .f = function(z, x) {all(result$row_roles[[x]] == task$row_roles[[x]])}))
    ))
    expect_true(
      all(map_lgl(c("weights", "groups", "strata", "nrow"), function(x) {
        all(result[[x]] == task[[x]])
    })))
  }
  task = mlr_tasks$get("iris")
  check_result_regr(convert_task(task, new_target = "Sepal.Width", new_type = "regr", drop_original_target = TRUE))
})

test_that("convert_task - same target", {
  check_result_regr = function(task, result) {
    expect_class(result, "TaskRegr")
    expect_task(result)
    expect_true(result$col_roles$target == "medv")
    expect_true(all(unlist(imap(result$row_roles,
      .f = function(z, x) {all(result$row_roles[[x]] == task$row_roles[[x]])}))
    ))
    expect_true(
      all(map_lgl(
        c("weights", "groups", "strata", "nrow", "ncol", "feature_names", "target_names",
          "task_type"),
        function(x) {all(result[[x]] == task[[x]])}
      )))
  }
  task = mlr_tasks$get("boston_housing")
  task$row_roles$use = setdiff(1:40, task$row_roles$use)
  task$row_roles$validation = 1:40
  task$col_roles$feature = setdiff(task$col_roles$feature, "lat")
  check_result_regr(convert_task(task, new_target = "medv", new_type = "regr", drop_original_target = TRUE), task)
  check_result_regr(convert_task(task, new_target = "medv", new_type = "regr", drop_original_target = FALSE), task)
})

test_that("convert task - general checks", {
  btask = mlr_tasks$get("boston_housing")
  itask = mlr_tasks$get("iris")

  # target does not exist
  expect_error(convert_task(btask, new_target = "medv2"))

  # target class does not match
  expect_error(convert_task(btask, new_target = "medv", new_type = "classif"))
  expect_error(convert_task(itask, new_target = "Sepal.Length", new_type = "classif"))
})

test_that("convert_task reconstructs task", {
  task = mlr_tasks$get("iris")
  tsk = convert_task(task)
  tsk$man = "mlr3::mlr_tasks_iris"
  expect_equal(task, tsk, check.attributes = FALSE)

  task2 = task$filter(1:100)
  tsk2 = convert_task(task2, drop_levels = TRUE)
  expect_equal(task2$nrow, tsk2$nrow)
  expect_equal(task2$ncol, tsk2$ncol)
  expect_true("twoclass" %in% tsk2$properties)

  task3 = task2
  task3$row_roles$use = 1:150
  tsk3 = convert_task(task3)
  tsk3$man = "mlr3::mlr_tasks_iris"
  expect_equal(task3$nrow, tsk3$nrow)
  expect_equal(task3$ncol, tsk3$ncol)
  expect_true("multiclass" %in% tsk3$properties)
  expect_equal(task, tsk3, check.attributes = FALSE)
})
