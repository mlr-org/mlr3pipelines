context("PipeOpLearnerCV")

test_that("PipeOLearnerCV - basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerCV$new(lrn)
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 1)
  expect_data_table(po$output, nrow = 1)

  task = mlr_tasks$get("iris")
  result = train_pipeop(po, list(task = task))
  expect_class(result[[1L]], "Task")

  result = predict_pipeop(po, list(task = task))
  expect_class(result[[1L]], "Prediction")
})
