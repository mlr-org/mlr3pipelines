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
  tsk = Task$new("hurdle", task_type = "regr",  DataBackendDataTable$new(dt, "..row_id"))
  tsk$set_col_role("y", "target")

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

  e$train(1:150)
  e$predict(151:200)
}
