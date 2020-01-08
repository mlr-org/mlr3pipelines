
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
