context("usecases for pipelines")

test_graph = function(g, n_nodes, n_edges) {

  g$keep_results = TRUE
  task = mlr_tasks$get("iris")
  expect_graph(g, n_nodes = n_nodes, n_edges = n_edges)
  res.train = g$train(task)
  outlist = list(NULL)
  names(outlist) = g$output$name
  expect_equal(res.train, outlist)
  g.trained = g
  g = g$clone(deep = TRUE)
  expect_true(g$is_trained)
  res.pred = g$predict(task)
  expect_list(res.pred, types = "Prediction")
  expect_equal(names(res.pred), g$output$name)
  list(g.trained = g.trained, g.predicted = g)
}

test_that("linear: scale + pca + learn", {
  g = PipeOpScale$new() %>>% PipeOpPCA$new() %>>% PipeOpLrnRP
  z = test_graph(g, n_nodes = 3L, n_edges = 2L)

  expect_equal(z$g.trained$pipeops$scale$.result[[1]]$data(cols = colnames(iris)[1:4]),
    as.data.table(scale(iris[1:4])))

  expect_equal(abs(as.matrix(z$g.trained$pipeops$pca$.result[[1]]$data(cols = paste0("PC", 1:4)))),
    abs(prcomp(scale(iris[1:4]))$x))

  expect_equal(z$g.trained$pipeops$classif.rpart$.result, list(NULL))

  expect_equal(z$g.predicted$pipeops$scale$.result[[1]]$data(cols = colnames(iris)[1:4]),
    as.data.table(scale(iris[1:4])))

  expect_equal(abs(as.matrix(z$g.predicted$pipeops$pca$.result[[1]]$data(cols = paste0("PC", 1:4)))),
    abs(prcomp(scale(iris[1:4]))$x))

  expect_equal(z$g.predicted$pipeops$classif.rpart$.result, unname(z$g.trained$predict(mlr_tasks$get("iris"))))
})

test_that("featureunion", {
  g = gunion(list(PipeOpPCA$new(), PipeOpNOP$new())) %>>%
    PipeOpFeatureUnion$new(2L) %>>% PipeOpLrnRP
  z = test_graph(g, n_nodes = 4L, n_edges = 3L)

  expect_equal(abs(as.matrix(z$g.trained$pipeops$pca$.result[[1]]$data(cols = paste0("PC", 1:4)))),
    abs(prcomp(iris[1:4])$x))

  expect_equal(z$g.trained$pipeops$nop$.result[[1]], mlr_tasks$get("iris"))

  expect_equal(abs(as.matrix(z$g.trained$pipeops$featureunion$.result[[1]]$data(cols = c(paste0("PC", 1:4), colnames(iris)[1:4])))),
    as.matrix(cbind(abs(prcomp(iris[1:4])$x), iris[1:4])))
})

# FIXME: have a look at intermediate results in all usecase, we should expect some stuff there

test_that("bagging", {
  g = greplicate(PipeOpSubsample$new() %>>% PipeOpLrnRP, 2L) %>>% PipeOpMajorityVote$new(innum = 2L)
  g$pipeops$subsample_1$param_set$values$frac = .5
  g$pipeops$subsample_2$param_set$values$frac = .5
  z = test_graph(g, n_nodes = 5L, n_edges = 4L)

  expect_equal(z$g.trained$pipeops$classif.rpart_1$.result, list(NULL))
  expect_equal(z$g.trained$pipeops$classif.rpart_2$.result, list(NULL))
  expect_equal(z$g.trained$pipeops$majorityvote$.result, list(NULL))
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

  expect_equal(z$g.trained$pipeops$classif.rpart$.result, list(NULL))
  expect_equal(z$g.trained$pipeops$classif.featureless$.result, list(output = NO_OP))
  expect_equal(z$g.trained$pipeops$unbranch$.result, list(input1 = NULL))

  expect_equal(z$g.predicted$pipeops$classif.rpart$.result[[1]], z$g.predicted$pipeops$unbranch$.result[[1]])
  expect_equal(z$g.predicted$pipeops$classif.featureless$.result, list(output = NO_OP))

  g = PipeOpBranch$new(2L) %>>% gunion(list(PipeOpLrnRP, PipeOpLrnFL)) %>>% PipeOpUnbranch$new(2L)
  task = mlr_tasks$get("iris")
  res = g$train(task)
  expect_true(g$is_trained)
  expect_equal(res, list(unbranch.output = NULL))
  res = g$predict(task)
  expect_list(res, types = "Prediction")
  expect_equal(names(res), "unbranch.output")
})

test_that("branching with varargs", {
  g = PipeOpBranch$new(2L) %>>% gunion(list(PipeOpLrnRP, PipeOpLrnFL)) %>>% PipeOpUnbranch$new()
  z = test_graph(g, n_nodes = 4L, n_edges = 4L)

  expect_equal(z$g.trained$pipeops$classif.rpart$.result, list(NULL))
  expect_equal(z$g.trained$pipeops$classif.featureless$.result, list(output = NO_OP))
  expect_equal(z$g.trained$pipeops$unbranch$.result, list("..." = NULL))

  expect_equal(z$g.predicted$pipeops$classif.rpart$.result[[1]], z$g.predicted$pipeops$unbranch$.result[[1]])
  expect_equal(z$g.predicted$pipeops$classif.featureless$.result, list(output = NO_OP))

  g = PipeOpBranch$new(2L) %>>% gunion(list(PipeOpLrnRP, PipeOpLrnFL)) %>>% PipeOpUnbranch$new()
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
    PipeOpNOP$new()))
  pipe = pipe %>>% PipeOpFeatureUnion$new(3)

  result = pipe$train(task)[[1]]

  expect_task(result)

  expect_set_equal(result$feature_names, c("classif.rpart.response", "classif.featureless.response", task$feature_names))

  task_p = mlr_tasks$get("iris")$filter(1:10)
  result_predict = pipe$predict(task_p)[[1]]

  expect_set_equal(result_predict$feature_names, c("classif.rpart.response", "classif.featureless.response", task$feature_names))

  pipe$pipeops$classif.rpart$learner$predict_type = "prob"
  pipe$pipeops$classif.featureless$learner$predict_type = "prob"
  pipe$pipeops$classif.featureless$values$resampling.keep_response = TRUE

  result = pipe$train(task)[[1]]

  expect_set_equal(result$feature_names,
    c(paste0("classif.rpart.prob.", task$class_names),
      paste0("classif.featureless.prob.", task$class_names),
      "classif.featureless.response",
      task$feature_names))

  result_predict = pipe$predict(task_p)[[1]]

  expect_set_equal(result_predict$feature_names,
    c(paste0("classif.rpart.prob.", task$class_names),
      paste0("classif.featureless.prob.", task$class_names),
      "classif.featureless.response",
      task$feature_names))
})
