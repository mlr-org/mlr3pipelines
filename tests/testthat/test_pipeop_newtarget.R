test_that("PipeOpNewTarget -  Regr -> Regr", {
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
  op = PipeOpNewTarget$new(param_vals = list(new_target = "age", new_task_type = "regr"))
  task = mlr_tasks$get("boston_housing")
  expect_pipeop(op)
  tresult = train_pipeop(op, inputs = list(task))[[1]]
  check_result_regr(tresult)
  presult = predict_pipeop(op, inputs = list(task))[[1]]
  check_result_regr(presult)
})

test_that("PipeOpNewTarget - Regr -> Classif", {
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
  op = PipeOpNewTarget$new(param_vals = list(new_target = "chas", new_task_type = "classif"))
  task = mlr_tasks$get("boston_housing")
  expect_pipeop(op)

  tresult = train_pipeop(op, inputs = list(task))[[1]]
  check_result_classif(tresult)
  presult = predict_pipeop(op, inputs = list(task))[[1]]
  check_result_classif(presult)
})


test_that("PipeOpNewTarget - Classif -> Regr", {
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
  op = PipeOpNewTarget$new(param_vals = list(new_target = "Sepal.Width", new_task_type = "regr"))
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  tresult = train_pipeop(op, inputs = list(task))[[1]]
  check_result_regr(tresult)
  presult = predict_pipeop(op, inputs = list(task))[[1]]
  check_result_regr(presult)
})


test_that("UseCase - Hurdle Model", {
  xtmp = rnorm(200)
  dt = data.table("x1" = rnorm(200), "x2" = rnorm(200))
  # We have a process where y = 0
  dt[, y := ifelse(x1 * 0.2 + x2 * 0.4 - 0.3 > 0, 0, 2 + x1 * x2 + 0.5 * x2)]
  dt[, ..row_id := seq_len(nrow(dt))]
  tsk = TaskRegr$new("hurdle", DataBackendDataTable$new(dt, "..row_id"), target = "y")

  op_ntgt = PipeOpMutateTarget$new("ytmp", list(mutation = list(y_tmp = ~ factor(y > 0, levels = c(TRUE, FALSE)))))
  expect_pipeop(op_ntgt)

  op_set_ytmp = PipeOpNewTarget$new("set_ytmp", param_vals = list(new_target = "y_tmp", new_task_type = "classif"))
  expect_pipeop(op_set_ytmp)

  op_set_y = PipeOpNewTarget$new("set_y", param_vals = list(new_target = "y", new_task_type = "regr"))
  expect_pipeop(op_set_y)

  op_cvlrn = PipeOpLearnerCV$new(id = "cvlrn", mlr_learners$get("classif.rpart"))
  op_lrn = PipeOpLearner$new(mlr_learners$get("regr.rpart"))

  hurdle = op_ntgt %>>% op_set_ytmp %>>% op_cvlrn %>>% op_set_y

  pipe = PipeOpCopy$new(2) %>>%
    gunion(list(PipeOpNOP$new(), hurdle)) %>>%
    PipeOpFeatureUnion$new(2) %>>%
    op_lrn
  expect_graph(pipe)

  e = resample(task = tsk, learner = GraphLearner$new(pipe, task_type = "regr"), resampling = rsmp("holdout"))
  expect_class(e, "ResampleResult")
  expect_true(e$score()$regr.mse < 1)
})
