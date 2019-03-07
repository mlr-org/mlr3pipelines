test_that("PipeOpNewTarget - basic properties", {
  op = PipeOpNewTarget$new()
  task = mlr_tasks$get("bh")
  expect_pipeop(op)
  op$new_target = "age"
  result = train_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
  expect_true(result[[1]]$col_roles$target == "age")
  result = predict_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
})


test_that("UseCase - Hurdle Model", {

  dt = data.table("x1" = rnorm(200), "x2" = rnorm(200))
  # We have a process where y = 0
  dt[, y := ifelse(x1 * x2 < 1, 0, 2 + x1 * x2 + 0.5 * x2)]
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
  pipe = op_set_ytmp %>>% PipeOpCopy$new(2) %>>% gunion(list(PipeOpNULL$new(), op_cvlrn))  %>>% PipeOpFeatureUnion$new(2) %>>% op_set_y %>>% op_lrn

  e = Experiment$new(task = tsk,  learner = GraphLearner$new(pipe, task_type = "regr"))
  e$train()
  e$predict()
  e$score()
}

test_that("UseCase - MultiOutput - Chained", {
  # Example task
  dt = data.table("x1" = rnorm(100), "x2" = rnorm(100))
  dt[, m_class := x1 * x2]
  dt[, x_class := x1 * 2]
  dt[, c_class := factor(m_class + x_class * x1 > 3)]
  dt[, ..row_id := seq_len(nrow(dt))]
  tsk = TaskRegr$new("multiout", DataBackendDataTable$new(dt, "..row_id"), target = "x_class")
  # c is classif, m and x are regr targets; x_class is the final output we want to predict
  tsk$set_col_role(c("m_class", "c_class"), "unused")
  tsk$set_row_role(71:100, "validation")

  op_c = PipeOpNewTarget$new("set_c")
  op_c$new_target = "c_class"
  op_c$new_task_type = "classif"
  op_m = PipeOpNewTarget$new("set_m")
  op_m$new_target = "m_class"
  op_m$new_task_type = "regr"
  op_x = PipeOpNewTarget$new("set_x")
  op_x$new_target = "x_class"
  op_x$new_task_type = "regr"

  op_cvlrn_c = PipeOpLearnerCV$new(id = "c_cvlrn", mlr_learners$get("classif.rpart"))
  op_cvlrn_m = PipeOpLearnerCV$new(id = "m_cvlrn", mlr_learners$get("regr.rpart"))
  op_lrn_x = PipeOpLearner$new(mlr_learners$get("regr.rpart"))

  pipe = op_c %>>%
    PipeOpCopy$new(2) %>>% gunion(list(PipeOpNULL$new(), op_cvlrn_c)) %>>%
    PipeOpFeatureUnion$new(2) %>>%
    op_m %>>%
    PipeOpCopy$new(id = "copy2", 2) %>>% gunion(list(PipeOpNULL$new(id = "null2"), op_cvlrn_m)) %>>%
    PipeOpFeatureUnion$new(id = "funion2", 2) %>>%
    op_x %>>%
    op_lrn_x

  e = Experiment$new(task = tsk, learner = GraphLearner$new(pipe, task_type = "regr"))
  e$train()
  e$predict()
  e$score()
}
