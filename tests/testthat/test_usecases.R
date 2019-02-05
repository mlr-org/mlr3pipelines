context("usecases for pipelines")

test_graph = function(g, n_nodes, n_edges) {
  task = mlr_tasks$get("iris")
  expect_graph(g, n_nodes = n_nodes, n_edges = n_edges)
  res.train = g$train(task)
  outlist = list(NULL)
  names(outlist) = g$output$name
  expect_equal(res.train, outlist)
  g.trained = g$clone()
  expect_true(g$is_trained)
  res.pred = g$predict(task)
  expect_list(res.pred, types = "Prediction")
  expect_equal(names(res.pred), g$output$name)
  g.predicted = g$clone()
  list(g.trained = g.trained, g.predicted = g.predicted)
}

test_that("linear: scale + pca + learn", {
  g = PipeOpScale$new() %>>% PipeOpPCA$new() %>>% PipeOpLrnRP
  z = test_graph(g, n_nodes = 3L, n_edges = 2L)
})

test_that("featureunion", {
  g = gunion(list(PipeOpPCA$new(), PipeOpNULL$new())) %>>%
    PipeOpFeatureUnion$new(2L) %>>% PipeOpLrnRP
  z = test_graph(g, n_nodes = 4L, n_edges = 3L)
})

#FIXME: have a look at intermediate results in all usecase, we should expect some stuff there

test_that("bagging", {
  g = greplicate(PipeOpDownsample$new() %>>% PipeOpLrnRP, 2L) %>>% PipeOpMajorityVote$new(innum = 2L)
  z = test_graph(g, n_nodes = 5L, n_edges = 4L)
})


test_that("branching", {
  # FIXME: are we REALLY sure that stuff here gets connected in the correct order?
  # i doubt that and this looks really bad and errorprone
  # b) we really want to have an associated order in the graph which is determined by
  # the nodes in the pipeops, not lexical by id or something else
  # c) we need a delayed init of the pipeop, which is triggered when the pipeop is
  # connected. this  would help a lot when we connect the PipeOp to its neighbor and
  # need some info from the nieghbor to decide some poperties of the PO
  g = PipeOpBranch$new(2L) %>>% gunion(list(PipeOpLrnRP, PipeOpLrnFL)) %>>% PipeOpUnbranch$new(2L)
  z = test_graph(g, n_nodes = 4L, n_edges = 4L)

  task = mlr_tasks$get("iris")
  res = g$train(task)
  expect_true(g$is_trained)
  expect_equal(res, list(unbranch.output = NULL))
  res = g$predict(task)
  expect_list(res, types = "Prediction")
  expect_equal(names(res), "unbranch.output")
})


test_that("task chunking", {
  g = PipeOpChunk$new(2L) %>>% greplicate(PipeOpLrnRP, 2L) %>>% PipeOpMajorityVote$new(2L)
  z = test_graph(g, n_nodes = 4L, n_edges = 4L)
})


test_that("stacking", {
  task = mlr_tasks$get("iris")

  lrn1 = mlr_learners$get("classif.rpart")
  lrn2 = mlr_learners$get("classif.featureless")
  pipe = gunion(list(
    PipeOpLearnerCV$new(lrn1),
    PipeOpLearnerCV$new(lrn2),
    PipeOpNULL$new()))
  pipe = pipe %>>% PipeOpFeatureUnion$new(3)

  result = pipe$train(task)[[1]]

  expect_task(result)

  expect_set_equal(result$feature_names, c("rpart.response", "featureless.response", task$feature_names))

  task_p = mlr_tasks$get("iris")$filter(1:10)
  result_predict = pipe$predict(task_p)[[1]]

  expect_set_equal(result_predict$feature_names, c("rpart.response", "featureless.response", task$feature_names))

  pipe$pipeops$rpart$learner$predict_type = "prob"
  pipe$pipeops$featureless$learner$predict_type = "prob"

  result = pipe$train(task)[[1]]

  expect_set_equal(result$feature_names,
    c(paste0("rpart.prob.", task$class_names),
      "rpart.response",
      paste0("featureless.prob.", task$class_names),
      "featureless.response",
      task$feature_names))

  result_predict = pipe$predict(task_p)[[1]]

  expect_set_equal(result_predict$feature_names,
    c(paste0("rpart.prob.", task$class_names),
      "rpart.response",
      paste0("featureless.prob.", task$class_names),
      "featureless.response",
      task$feature_names))

})

