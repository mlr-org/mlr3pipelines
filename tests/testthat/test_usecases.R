#FIXME: pipeops shouldnt have an ID in their constructor. the ID should be an AB.

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
  expect_graph(g, n_nodes = 4L, n_edges = 3L)

  g$train(task)
  # # FIXME: also do a predict
  # # graph$predict(task)
  # FIXME: at least roughly check that correct stuff is returned
})

#FIXME: have a look at intermediate results in all usecase, we should expect some stuff there

test_that("bagging", {
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")

  g = greplicate(PipeOpDownsample$new() %>>% PipeOpLearner$new(lrn), 2L) %>>%
    PipeOpModelAvg$new(innum = 2L)
  expect_graph(g, n_nodes = 5L, n_edges = 4L)

  g$train(task)
  g$predict(task)
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
  # expect_graph(g, n_nodes = 5L, n_edges = 4L)
   #FIXME: finish test
  # g$train(list(task))
  # graph$predict(task)
})








