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
  op$new_task_type = "regr"
  result = train_pipeop(op, inputs = list(task))[[1]]
  expect_task(result)
  expect_true(result$col_roles$target == "Sepal.Width")
  expect_true(all(result$feature_names != "Sepal.Width"))
  expect_true(all(result$feature_names != "Species"))
  expect_class(result, "TaskRegr")
  result = predict_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
})


test_that("PipeOpNewTarget - Edge cases", {
  op = PipeOpNewTarget$new()
  task = mlr_tasks$get("iris")
  op$new_target = "Species"
  op$new_task_type = "classif"
  expect_pipeop(op)
  result = train_pipeop(op, inputs = list(task))[[1]]
  expect_task(result)
  expect_true(result$col_roles$target == "Species")
  expect_class(result, "TaskClassif")
  result = predict_pipeop(op, inputs = list(task))[[1]]
  expect_task(result)
  expect_true(result$col_roles$target == "Species")
  expect_class(result, "TaskClassif")
})

test_that("UseCase - Hurdle Model", {

  xtmp = rnorm(200)
  dt = data.table("x1" = rnorm(200), "x2" = rnorm(200))
  # We have a process where y = 0
  dt[, y := ifelse(x1 * 0.2 + x2 * 0.4 - 0.3 > 0, 0, 2 + x1 * x2 + 0.5 * x2)][, y := ifelse(y < 1, 0, y)]
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

  e = resample(task = tsk, learner = GraphLearner$new(pipe, task_type = "regr"), resampling = "holdout")
  expect_class(e, "ResampleResult")
  expect_true(e$performance()$regr.mse < 1)
})


## This test currently does not work, as PipeOpMutate currently does not allow to modify the target.
#  FIXME: Decide whether this should work.
# test_that("UseCase - Hurdle Model II", {

#   xtmp = rnorm(200)
#   dt = data.table("x1" = rnorm(200), "x2" = rnorm(200))
#   dt[, y := ifelse(x1 * 0.2 + x2 * 0.4 - 0.3 > 0, 0, 2 + x1 * x2 + 0.5 * x2)][, y := ifelse(y < 2, 0, y)]
#   dt[, ..row_id := seq_len(nrow(dt))]
#   tsk = TaskRegr$new("hurdle", DataBackendDataTable$new(dt, "..row_id"), target = "y")
#   tsk$set_row_role(151:200, "validation")

#   pom = PipeOpMutate$new("fct_y", list(mutation = list(y_tmp = ~ y > 0)))
#   po_set_ytmp = PipeOpNewTarget$new("set_ytmp", "y_tmp", "classif")
#   op_cvlrn = PipeOpLearnerCV$new(id = "cvlrn", mlr_learners$get("classif.rpart"))
#   op_lrn = PipeOpLearner$new(mlr_learners$get("regr.rpart"))
#   po_set_y = PipeOpNewTarget$new("set_y", "y", "regr")

#   pipe = pom %>>% po_set_ytmp %>>%
#     PipeOpCopy$new(2) %>>% gunion(list(PipeOpNOP$new(), op_cvlrn)) %>>% PipeOpFeatureUnion$new(2) %>>%
#     po_set_y %>>% op_lrn

#   pom$train(list(tsk))

#   e = resample(task = tsk, learner = GraphLearner$new(pipe, task_type = "regr"), resampling = "holdout")
#   expect_class(e, "ResampleResult")
#   expect_true(e$performance()$regr.mse < 1)
# })


test_that("UseCase - MultiOutput - Chained", {
  # Example task
  xtmp = rnorm(100)
  dt = data.table("x1" = rnorm(100), "x2" = rnorm(100))
  dt[, m_class := x1 * x2 + xtmp]
  dt[, x_class := x1 * 2 * xtmp + 1]
  dt[, c_class := factor(m_class + x_class * x1 > 1)]
  dt[, ..row_id := seq_len(nrow(dt))]
  tsk = TaskRegr$new("multiout", DataBackendDataTable$new(dt, "..row_id"), target = "x_class")
  # c is classif, m and x are regr targets; x_class is the final output we want to predict
  tsk$set_col_role(c("m_class", "c_class"), "unused")
  tsk$set_row_role(71:100, "validation")

  # We create PipeOp's that set a new intermediate target
  op_c = PipeOpNewTarget$new("set_c")
  op_c$new_target = "c_class"
  op_m = PipeOpNewTarget$new("set_m")
  op_m$new_target = "m_class"
  op_x = PipeOpNewTarget$new("set_x")
  op_x$new_target = "x_class"

  op_cvlrn_c = PipeOpLearnerCV$new(id = "c_cvlrn", mlr_learners$get("classif.rpart"))
  op_cvlrn_m = PipeOpLearnerCV$new(id = "m_cvlrn", mlr_learners$get("regr.rpart"))
  op_lrn_x =   PipeOpLearner$new(mlr_learners$get("regr.rpart"))

  pipe = op_c %>>% # Change to target c_class
    PipeOpCopy$new(2) %>>% gunion(list(PipeOpNOP$new(), op_cvlrn_c)) %>>% # cvlearn on c_class
    PipeOpFeatureUnion$new(2) %>>%
    op_m %>>% # change to m_class
    PipeOpCopy$new(id = "copy2", 2) %>>% gunion(list(PipeOpNOP$new(id = "null2"), op_cvlrn_m)) %>>%
    PipeOpFeatureUnion$new(id = "funion2", 2) %>>% # cvlearn on m_class
    op_x %>>% # change to x_class
    op_lrn_x

  e = resample(task = tsk, learner = GraphLearner$new(pipe, task_type = "regr"), resampling = "holdout")
  expect_class(e, "ResampleResult")
  expect_true(e$performance()$regr.mse < 5)

  # Compare to no intermediate targets
  e = resample(task = tsk, learner = "regr.rpart", resampling = "holdout")
  expect_class(e, "ResampleResult")
})
