context("resample")

test_that("PipeOp - Resample", {
  skip_if_not_installed("rpart")
  task = mlr_tasks$get("iris")
op1 = po("scale")
  lrn = mlr_learners$get("classif.rpart")
op2 = po("learner", lrn)
  pp = GraphLearner$new(op2)
  resa = mlr_resamplings$get("cv")
  resa$param_set$values$folds = 2L

  rr = mlr3::resample(task, pp, resa)
  assert_resample_result(rr)
  expect_class(rr, "ResampleResult")
})
