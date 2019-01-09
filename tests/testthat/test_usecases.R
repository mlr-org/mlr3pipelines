context("usecases for pipelines")

test_that("linear: scale + pca", {
  # check that we can train and predict, on the same data, result should be the same
  task = mlr_tasks$get("iris")
  g = PipeOpScale$new() %>>% PipeOpPCA$new()
  expect_graph(g)
  res1 = g$train(task)
  assert_list(res1, types = "Task")
  res2 = g$predict(task)
  assert_list(res2, types = "Task")
  expect_equal(res1[[1]]$data(), res2[[1]]$data())
})


test_that("featureunion", {
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")

  g = gunion(list(PipeOpPCA$new(), PipeOpNULL$new())) %>>% PipeOpFeatureUnion$new(2L) %>>% PipeOpLearner$new(learner = lrn)
  expect_false(g$is_trained)
  expect_graph(g)

  g$train(task)
  # # FIXME: also do a predict
  # # graph$predict(task)
  # FIXME: at least roughly check that correct stuff is returned
})

#FIXME: have a look at intermediate results in all usecase, we should expect some stuff there

test_that("bagging", {
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")

  # FIXME: modelavg is not there!
  # g = greplicate(PipeOpDownsample$new() %>>% PipeOpLearner$new(lrn), 2L) %>>% PipeOpModelAvg()
  # expect_graph(g)

  # g$train(list(task))
  # # FIXME: also do a predict
  # # graph$predict(task)
  # FIXME: at least roughly check that correct stuff is returned
})







