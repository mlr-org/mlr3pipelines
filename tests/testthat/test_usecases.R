#FIXME: pipeops shouldnt have an ID in their constructor. the ID should be an AB.

# FIXME: maybe we can do the train/predict/expect stuff for the graph in a helper file?

context("usecases for pipelines")

test_that("linear: scale + pca + learn", {
  # check that we can train and predict, on the same data, result should be the same
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")
  g = PipeOpScale$new() %>>% PipeOpPCA$new() %>>% PipeOpLearner$new(learner = lrn)
  expect_graph(g, n_nodes = 3L, n_edges = 2L)
  res = g$train(task)
  expect_true(g$is_trained)
  expect_equal(res, list(NULL))
  res = g$predict(task)
  assert_list(res, types = "Prediction")
})


test_that("featureunion", {
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")

  g = gunion(list(PipeOpPCA$new(), PipeOpNULL$new())) %>>%
    PipeOpFeatureUnion$new(2L) %>>% PipeOpLearner$new(learner = lrn)
  expect_graph(g, n_nodes = 4L, n_edges = 3L)

  res = g$train(task)
  expect_true(g$is_trained)
  expect_equal(res, list(NULL))
  res = g$predict(task)
  expect_list(res, types = "Prediction")
})

#FIXME: have a look at intermediate results in all usecase, we should expect some stuff there

test_that("bagging", {
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")

  g = greplicate(PipeOpDownsample$new() %>>% PipeOpLearner$new(lrn), 2L) %>>%
    PipeOpMajorityVote$new(innum = 2L)
  expect_graph(g, n_nodes = 5L, n_edges = 4L)

  res = g$train(task)
  expect_true(g$is_trained)
  expect_equal(res, list(NULL))
  res = g$predict(task)
  expect_list(res, types = "Prediction")
})


test_that("branching", {
  task = mlr_tasks$get("iris")
  lrn1 = mlr_learners$get("classif.rpart")
  lrn2 = mlr_learners$get("classif.featureless")

  # FIXME: are we REALLY sure that stuff here gets connected in the correct order?
  # i doubt that and this looks really bad and errorprone
  # b) we really want to have an associated order in the graph which is determined by
  # the nodes in the pipeops, not lexical by id or something else
  # c) we need a delayed init of the pipeop, which is triggered when the pipeop is
  # connected. this  would help a lot when we connect the PipeOp to its neighbor and
  # need some info from the nieghbor to decide some poperties of the PO
  g = PipeOpBranch$new(2L) %>>%
    gunion(list(PipeOpLearner$new(lrn1), PipeOpLearner$new(lrn2))) %>>%
    PipeOpUnbranch$new(2L)
  expect_graph(g, n_nodes = 4L, n_edges = 4L)

  #FIXME: test currently fails and needs to be reenabled
  # res = g$train(task)
  # expect_true(g$is_trained)
  # expect_equal(res, list(NULL))
  # res = g$predict(task)
  # expect_list(res, types = "Prediction")
})


test_that("task chunking", {
  task = mlr_tasks$get("iris")
  lrn1 = mlr_learners$get("classif.rpart")

  g = PipeOpChunk$new(2L) %>>% greplicate(PipeOpLearner$new(lrn), 2L) %>>%
    PipeOpMajorityVote$new(2L)
  expect_graph(g, n_nodes = 4L, n_edges = 4L)

  res = g$train(task)
  expect_true(g$is_trained)
  expect_equal(res, list(NULL))
  res = g$predict(task)
  expect_list(res, types = "Prediction")
})





