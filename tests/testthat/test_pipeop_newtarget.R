test_that("PipeOpNewTarget -  Regr -> Regr", {
  op = PipeOpNewTarget$new()
  task = mlr_tasks$get("boston_housing")
  expect_pipeop(op)
  op$new_target = "age"
  result = train_pipeop(op, inputs = list(task))[[1]]
  expect_class(result, "TaskRegr")
  expect_task(result)
  expect_true(result$col_roles$target == "age")
  expect_true(all(result$feature_names != "age"))
  expect_true(all(result$feature_names != "medv"))
  result = predict_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
})

test_that("PipeOpNewTarget - Regr -> Classif", {
  op = PipeOpNewTarget$new()
  task = mlr_tasks$get("boston_housing")
  expect_pipeop(op)
  op$new_target = "chas"
  result = train_pipeop(op, inputs = list(task))[[1]]
  expect_task(result)
  expect_true(result$col_roles$target == "chas")
  expect_true(all(result$feature_names != "chas"))
  expect_true(all(result$feature_names != "medv"))
  expect_class(result, "TaskClassif")
  result = predict_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
})


test_that("PipeOpNewTarget - Classif -> Regr", {
  op = PipeOpNewTarget$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  op$new_target = "Sepal.Width"
  result = train_pipeop(op, inputs = list(task))[[1]]
  expect_task(result)
  expect_true(result$col_roles$target == "Sepal.Width")
  expect_true(all(result$feature_names != "Sepal.Width"))
  expect_true(all(result$feature_names != "Species"))
  expect_class(result, "TaskRegr")
  result = predict_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
})


test_that("UseCase - Hurdle Model", {

  xtmp = rnorm(200)
  dt = data.table("x1" = rnorm(200), "x2" = rnorm(200))
  # We have a process where y = 0
  dt[, y := ifelse(x1 * 0.2 + x2 * 0.4 - 0.3 > 0, 0, 2 + x1 * x2 + 0.5 * x2)]
  dt[, y_tmp := as.factor(y > 0)]
  dt[, ..row_id := seq_len(nrow(dt))]
  tsk = TaskRegr$new("hurdle", DataBackendDataTable$new(dt, "..row_id"), target = "y")
  tsk$set_row_role(151:200, "validation")

  op_set_ytmp = PipeOpNewTarget$new("set_ytmp")
  op_set_ytmp$new_target = "y_tmp"
  op_set_ytmp$new_task_type = "classif"
  expect_pipeop(op_set_ytmp)

  op_set_y = PipeOpNewTarget$new("set_y")
  op_set_y$new_target = "y"
  op_set_y$new_task_type = "regr"
  expect_pipeop(op_set_y)

  op_cvlrn = PipeOpLearnerCV$new(id = "cvlrn", mlr_learners$get("classif.rpart"))
  op_lrn = PipeOpLearner$new(mlr_learners$get("regr.rpart"))
  pipe = op_set_ytmp %>>%
    PipeOpCopy$new(2) %>>% gunion(list(PipeOpNOP$new(), op_cvlrn)) %>>% PipeOpFeatureUnion$new(2) %>>%
    op_set_y %>>% op_lrn

  e = resample(task = tsk, learner = GraphLearner$new(pipe, task_type = "regr"), resampling = rsmp("holdout"))
  expect_class(e, "ResampleResult")
  expect_true(e$performance()$regr.mse < 1)
})


test_that("UseCase - Hurdle Model", {
  xtmp = rnorm(200)
  dt = data.table("x1" = rnorm(200), "x2" = rnorm(200))
  # We have a process where y = 0
  dt[, y := ifelse(x1 * 0.2 + x2 * 0.4 - 0.3 > 0, 0, 2 + x1 * x2 + 0.5 * x2)]
  dt[, ..row_id := seq_len(nrow(dt))]
  tsk = TaskRegr$new("hurdle", DataBackendDataTable$new(dt, "..row_id"), target = "y")
  # tsk$set_row_role(151:200, "validation")

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

  e = resample(task = tsk, learner = GraphLearner$new(pipe, task_type = "regr"), resampling = rsmp("holdout"))
  expect_class(e, "ResampleResult")
  expect_true(e$performance()$regr.mse < 1)
})
