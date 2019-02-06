context("resample")

test_that("PipeOp - Resample", {

  task = mlr_tasks$get("iris")
  op1 = PipeOpScale$new()
  lrn = mlr_learners$get("classif.rpart")
  op2 = PipeOpLearner$new(learner = lrn)
  pp = GraphLearner$new(op2)
  resa = mlr_resamplings$get("cv")
  resa$param_set$param_vals$folds = 2L

  rr = mlr3::resample(task, pp, resa)

  assert_resample_result(rr)
})
