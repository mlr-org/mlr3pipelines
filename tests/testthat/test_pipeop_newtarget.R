test_that("PipeOpNewTarget - Regr -> Regr", {
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

test_that("PipeOpNewTarget - Same target", {
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
  op = PipeOpNewTarget$new(param_vals = list(new_target = "medv", new_task_type = "regr"))
  task = mlr_tasks$get("boston_housing")
  task$set_row_role(1:40, "validation")
  task$set_col_role("lat", "unused")
  tresult = train_pipeop(op, inputs = list(task))[[1]]
  check_result_regr(task, tresult)
  presult = predict_pipeop(op, inputs = list(task))[[1]]
  check_result_regr(task, presult)
})

test_that("PipeOpNewTarget general checks", {
  btask = mlr_tasks$get("boston_housing")
  itask = mlr_tasks$get("iris")

  # Target does not exist
  op = PipeOpNewTarget$new(param_vals = list(new_target = "medv2", new_task_type = "regr"))
  expect_error(train_pipeop(op, inputs = list(btask))[[1]])

  # Target class does not match
  op = PipeOpNewTarget$new(param_vals = list(new_target = "medv", new_task_type = "classif"))
  expect_error(train_pipeop(op, inputs = list(btask))[[1]])
  op = PipeOpNewTarget$new(param_vals = list(new_target = "Sepal.Length", new_task_type = "classif"))
  expect_error(train_pipeop(op, inputs = list(itask))[[1]])
})

test_that("UseCase - Zero-Inflated Model", {
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

test_that("UseCase - Intermediate Target - Chained", {
  # Example task
  xtmp = rnorm(100)
  dt = data.table("x1" = rnorm(100), "x2" = rnorm(100))
  dt[, m_tgt := x2 + xtmp]
  dt[, c_tgt := factor(xtmp + x1 > 0)]
  dt[, x_tgt := m_tgt * x1]
  dt[, ..row_id := seq_len(nrow(dt))]
  tsk = TaskRegr$new("multiout", DataBackendDataTable$new(dt, "..row_id"), target = "x_tgt")
  tsk$set_col_role(c("m_tgt", "c_tgt"), "unused")
  # c is classif, m and x are regr targets; x_class is the final output we want to predict

  # We create PipeOp's that set a new intermediate target
  op_c = PipeOpNewTarget$new("op_c", param_vals = list(new_target = "c_tgt", new_task_type = "classif"))
  op_m = PipeOpNewTarget$new("op_m", param_vals = list(new_target = "m_tgt", new_task_type = "regr"))
  op_x = PipeOpNewTarget$new("op_x", param_vals = list(new_target = "x_tgt", new_task_type = "regr"))
  op_cvlrn_c = PipeOpLearnerCV$new(id = "c_cvlrn", mlr_learners$get("classif.rpart"))
  op_cvlrn_m = PipeOpLearnerCV$new(id = "m_cvlrn", mlr_learners$get("regr.rpart"))
  op_lrn_x =   PipeOpLearner$new(mlr_learners$get("regr.rpart"))


  prd_c = gunion(list(po("nop", "nop1"), op_c %>>% op_cvlrn_c)) %>>% po("featureunion", id = "union1", assert_targets_equal = FALSE)
  prd_m = gunion(list(po("nop", "nop2"), op_m %>>% op_cvlrn_m)) %>>% po("featureunion", id = "union2", assert_targets_equal = FALSE)
  prd_x = op_x %>>% op_lrn_x

  pipe = po("copy", id = "cp1", 2) %>>%
    prd_c %>>% po("copy", id = "cp2", 2) %>>%
    prd_m %>>%
    prd_x

  e = resample(task = tsk, learner = GraphLearner$new(pipe, task_type = "regr"), resampling = rsmp("holdout"))
  expect_class(e, "ResampleResult")
  expect_numeric(e$score()$regr.mse, lower = 0, upper = 10)

  # Compare to no intermediate targets
  e2 = resample(task = tsk, learner = lrn("regr.rpart"), resampling = rsmp("holdout"))
  expect_class(e2, "ResampleResult")
  expect_numeric(e2$score()$regr.mse, lower = 0, upper = 10)
})
