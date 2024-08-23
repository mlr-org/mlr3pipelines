context("GraphLearner")

test_that("basic graphlearner tests", {
  skip_if_not_installed("rpart")
  skip_on_cran()  # takes too long
  task = mlr_tasks$get("iris")

  lrn = mlr_learners$get("classif.rpart")
  gr = PipeOpLearner$new(lrn)

  glrn = GraphLearner$new(gr)
  expect_true(run_experiment(task, glrn)$ok)

  glrn = GraphLearner$new(gr)
  expect_learner(glrn)
  glrn$train(task)

  expect_prediction_classif({
    graphpred = glrn$predict(task)
  })
  expect_equal(graphpred,
    lrn$train(task)$predict(task))

  expect_prediction_classif({
    graphpred = glrn$predict(task)
  })
  expect_equal(graphpred, lrn$predict(task))

  set.seed(1)
  lrn = mlr_learners$get("classif.rpart")
  resgraphlrn = resample(task, lrn, mlr_resamplings$get("cv"))
  set.seed(1)
  resjustlrn = resample(task, lrn, mlr_resamplings$get("cv"))
  expect_equal(resgraphlrn$prediction(), resjustlrn$prediction())

  gr2 = PipeOpScale$new() %>>% PipeOpLearner$new(lrn)
  glrn2 = GraphLearner$new(gr2)
  glrn2_clone = glrn2$clone(deep = TRUE)
  expect_learner(glrn2)
  expect_true(run_experiment(task, glrn)$ok)
  glrn2$train(task)
  glrn2_clone$state = glrn2$state
#  glrn2_clone$state$log = glrn2_clone$state$log$clone(deep = TRUE)  # FIXME: this can go when mlr-org/mlr3#343 is fixed
#  glrn2_clone$state$model$classif.rpart$log = glrn2_clone$state$model$classif.rpart$log$clone(deep = TRUE)  # FIXME: this can go when mlr-org/mlr3#343 is fixed
  expect_deep_clone(glrn2_clone, glrn2$clone(deep = TRUE))
  expect_prediction_classif({
    graphpred2 = glrn2$predict(task)
  })

  expect_equal(glrn2$predict(task), glrn2_clone$predict(task))

  scidf = cbind(scale(iris[1:4]), iris[5])
  scalediris = TaskClassif$new("scalediris", as_data_backend(scidf), "Species")

  dblrn = mlr_learners$get("classif.debug")
  dblrn$param_set$values$save_tasks = TRUE

  dbgr = GraphLearner$new(PipeOpScale$new() %>>% PipeOpLearner$new(dblrn))


  expect_equal(dbgr$train(task), dbgr)

  # debuglearner predict() modifies model, but PipeOpLearner does not accept
  # model changes in predict phase, so would ordinarily discard the change.
  # Here we swap the debuglearner model by an environment, which gets updated
  # by-reference, so we can get the $task_predict slot eventually.
  dbmodels = as.environment(dbgr$model$classif.debug$model)
  dbgr$state$model$classif.debug$model = dbmodels

  dbgr$predict(task)

  expect_equal(dbmodels$task_train$data(), scalediris$data())
  expect_equal(dbmodels$task_predict$data(), scalediris$data())
})

test_that("GraphLearner clone_graph FALSE", {
  skip_if_not_installed("rpart")

  # prepare graph
  gr1 = po("pca") %>>% lrn("classif.rpart")
  gr1$train(tsk("iris"))
  expect_true(gr1$is_trained)

  gl = GraphLearner$new(gr1, clone_graph = FALSE)

  # graph is not cloned
  expect_identical(gl$graph, gr1)

  # GraphLearner$initialize resets graph state
  expect_false(gr1$is_trained)

  # compare result of training with a subset of iris
  gl$train(tsk("iris")$filter(1:110))

  # gr1 state is not set by this
  expect_false(gr1$is_trained)

  # train gr1 with a *different* task than gl
  gr1$train(tsk("iris"))

  # simulate pipeline with iris subset to get expected GraphLearner prediction result
  pp = po("pca")
  expected_prediction = lrn("classif.rpart")$train(pp$train(list(tsk("iris")$filter(1:110)))[[1]])$predict(pp$predict(list(tsk("iris")))[[1]])

  # check that predicting on iris subset gives different result from gr1$predict()
  expect_false(isTRUE(all.equal(gr1$predict(tsk("iris"))[[1]], expected_prediction)))
  expect_true(gr1$is_trained)

  # check that the GraphLearner predicts what we expect
  expect_true(isTRUE(all.equal(gl$predict(tsk("iris")), expected_prediction)))

  expect_false(gr1$is_trained)  # predicting with GraphLearner resets Graph state

  expect_identical(gl$graph, gr1)

  # check that as_learner respects `clone` now
  gl = as_learner(gr1, clone = FALSE)
  expect_identical(gl$graph, gr1)

})

test_that("graphlearner parameters behave as they should", {
  dblrn = mlr_learners$get("classif.debug")
  dblrn$param_set$values$save_tasks = TRUE

  dbgr = PipeOpScale$new() %>>% PipeOpLearner$new(dblrn)

  expect_subset(c("scale.center", "scale.scale", "classif.debug.x"), dbgr$param_set$ids())

  dbgr$param_set$values$classif.debug.x = 1

  expect_equal(dbgr$param_set$values$classif.debug.x, 1)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 1)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 1)

  dbgr$pipeops$classif.debug$param_set$values$x = 0

  expect_equal(dbgr$param_set$values$classif.debug.x, 0)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 0)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 0)

  dbgr$pipeops$classif.debug$learner$param_set$values$x = 0.5

  expect_equal(dbgr$param_set$values$classif.debug.x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 0.5)

  expect_error({
    dbgr$param_set$values$classif.debug.x = "a"
  })
  expect_error({
    dbgr$pipeops$classif.debug$param_set$values$x = "a"
  })
  expect_error({
    dbgr$pipeops$classif.debug$learner$param_set$values$x = "a"
  })

  expect_equal(dbgr$param_set$values$classif.debug.x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 0.5)

  dblrn = mlr_learners$get("classif.debug")
  dblrn$param_set$values$message_train = 1
  dblrn$param_set$values$message_predict = 1
  dblrn$param_set$values$warning_train = 1
  dblrn$param_set$values$warning_predict = 1

  pol = PipeOpLearner$new(dblrn, param_vals = list(message_predict = 0, warning_train = 0, warning_predict = 0))

  gl = GraphLearner$new(pol, param_vals = list(classif.debug.warning_train = 1, classif.debug.warning_predict = 1))

  gl$param_set$values$classif.debug.warning_predict = 0

  expect_mapequal(gl$param_set$values,
    list(classif.debug.message_predict = 0, classif.debug.message_train = 1, classif.debug.warning_predict = 0, classif.debug.warning_train = 1))
})

test_that("graphlearner type inference", {
  skip_if_not_installed("rpart")
  skip_on_cran()  # takes too long
  # default: classif
  lrn = GraphLearner$new(mlr_pipeops$get("nop"))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  ###########
  # classif #
  ###########

  # inference from pipeoplearner
  lrn = GraphLearner$new(mlr_pipeops$get("learner", lrn("classif.rpart")))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  # inference from output only
  lrn = GraphLearner$new(mlr_pipeops$get("copy", 1) %>>% mlr_pipeops$get("learner", lrn("classif.rpart")))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  # inference from input only
  lrn = GraphLearner$new(mlr_pipeops$get("learner", lrn("classif.rpart")) %>>% mlr_pipeops$get("copy", 1))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  # inference when multiple input, but one is a Task
  # inference when multiple input, but one is a Task

  lrn = GraphLearner$new(gunion(list(mlr_pipeops$get("learner", lrn("regr.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch"))
  expect_equal(lrn$task_type, "regr")
  # expect_equal(lrn$predict_type, "response")

  ###########
  # Errors  #
  ###########

  # input, output mismatching types
  gr = gunion(list(mlr_pipeops$get("learner", lrn("regr.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch") %>>% mlr_pipeops$get("learner", lrn("classif.rpart"))
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  gr = gunion(list(mlr_pipeops$get("learner", lrn("classif.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch") %>>% mlr_pipeops$get("learner", lrn("regr.rpart"))
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  # input two mismatching types
  gr = gunion(list(mlr_pipeops$get("learner", lrn("classif.rpart")), mlr_pipeops$get("learner", lrn("regr.rpart")))) %>>% mlr_pipeops$get("unbranch")
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  # input two mismatching types
  expect_error(GraphLearner$new(PipeOpScale$new()), "output type not.*Prediction.*or compatible")

  ###########################
  # Target Transformations  #
  ###########################

  lrn = GraphLearner$new(ppl("targettrafo", graph = lrn("classif.rpart"), trafo_pipeop = PipeOpTargetMutate$new()))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  lrn = GraphLearner$new(ppl("targettrafo", graph = lrn("regr.rpart"), trafo_pipeop = PipeOpTargetMutate$new()))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  lrn = GraphLearner$new(ppl("targettrafo", graph = lrn("regr.rpart"), trafo_pipeop = PipeOpTargetTrafoScaleRange$new()))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  ###########################
  # Synthetic examples      #
  ###########################
  # these construct broken graphs on purpose; if this gets caught somewhere else
  # in the future we might have to adjust the tests
  # The point here is that there could at some point be "legal" graphs that
  # end up using the fallback target type inference.


  g = po("branch", 2) %>>% gunion(list(mlr_pipeops$get("learner", lrn("regr.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch")
  g$pipeops$regr.rpart$output$predict = "*"
  expect_equal(as_learner(g)$task_type, "regr")
  g$pipeops$regr.rpart$input$train = "*"
  g$pipeops$regr.rpart$input$predict = "*"
  expect_equal(as_learner(g)$task_type, "regr")
  g$train(tsk("boston_housing"))
  expect_equal(as_learner(g)$task_type, "regr")  # should use base_learner inference

  g = po("replicate", reps = 2) %>>% lrn("regr.rpart") %>>% po("regravg", collect_multiplicity = TRUE)
  g$pipeops$regravg$input$predict = "[*]"
  g$pipeops$regravg$output$predict = "*"
  g$pipeops$regr.rpart$output$predict = "*"
  g$pipeops$regr.rpart$input$train = "*"
  g$pipeops$regr.rpart$input$predict = "*"
  expect_equal(as_learner(g)$task_type, "regr")
  expect_equal(as_learner(g)$task_type, "regr")  # should use base_learner inference
  g$train(tsk("boston_housing"))
  expect_equal(as_learner(g)$task_type, "regr")  # should not fail because of multiplicity in the graph

})

test_that("graphlearner type inference - branched", {
  skip_if_not_installed("rpart")
  skip_on_cran()  # takes too long

  # default: classif

  lrn = GraphLearner$new(gunion(list(
      mlr_pipeops$get(id = "l1", "learner", lrn("classif.rpart")),
      po("nop") %>>% mlr_pipeops$get(id = "l2", "learner", lrn("classif.rpart"))

    )) %>>%
    po("classifavg") %>>%
    po(id = "n2", "nop"))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  ###########
  # regr    #
  ###########

  # inference from pipeoplearner
  lrn = GraphLearner$new(mlr_pipeops$get("learner", lrn("regr.rpart")))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  # inference from output only
  lrn = GraphLearner$new(mlr_pipeops$get("copy", 1) %>>% mlr_pipeops$get("learner", lrn("regr.rpart")))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  # inference from input only
  lrn = GraphLearner$new(mlr_pipeops$get("learner", lrn("regr.rpart")) %>>% mlr_pipeops$get("copy", 1))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  # inference when multiple input, but one is a Task
  lrn = GraphLearner$new(gunion(list(
      mlr_pipeops$get(id = "l1", "learner", lrn("regr.rpart")),
      po("nop") %>>% mlr_pipeops$get(id = "l2", "learner", lrn("regr.rpart"))
    )) %>>%
    po("regravg") %>>%
    po(id = "n2", "nop"))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  ###########
  # Errors  #
  ###########

  # input, output mismatching types
  gr = gunion(list(mlr_pipeops$get("learner", lrn("regr.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch") %>>% mlr_pipeops$get("learner", lrn("classif.rpart"))
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  gr = gunion(list(mlr_pipeops$get("learner", lrn("classif.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch") %>>% mlr_pipeops$get("learner", lrn("regr.rpart"))
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  # input two mismatching types
  gr = gunion(list(mlr_pipeops$get("learner", lrn("classif.rpart")), mlr_pipeops$get("learner", lrn("regr.rpart")))) %>>% mlr_pipeops$get("unbranch")
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  # input two mismatching types
  expect_error(GraphLearner$new(PipeOpScale$new()), "output type not.*Prediction.*or compatible")

})

test_that("graphlearner predict type inference", {
  skip_if_not_installed("rpart")
  skip_on_cran()  # takes too long
  # Getter:

  # Classification
  lrp = po(lrn("classif.rpart", predict_type = "prob"))
  lrr = po(lrn("classif.rpart"))
  lfp = po(lrn("classif.featureless", predict_type = "prob"))
  lfr = po(lrn("classif.featureless"))
  nop = po("nop")

  # linear
  lrn = GraphLearner$new(lrp)
  expect_equal(lrn$predict_type, "prob")
  lrn = GraphLearner$new(lrr)
  expect_equal(lrn$predict_type, "response")
  lrn = GraphLearner$new(lrp %>>% nop)
  expect_equal(lrn$predict_type, "prob")

  # averager
  lrn = GraphLearner$new(pipeline_greplicate(po("subsample") %>>% lrr, 3L) %>>% po("classifavg"))
  expect_equal(lrn$predict_type, "response")
  lrn = GraphLearner$new(pipeline_greplicate(po("subsample") %>>% lrp, 3L) %>>% po("classifavg"))
  expect_equal(lrn$predict_type, "prob")

  # branching
  lrn = GraphLearner$new(po("branch", 2) %>>% gunion(list(lrp, lfp)) %>>% po("unbranch"))
  expect_equal(lrn$predict_type, "prob")
  lrn = GraphLearner$new(po("branch", 2) %>>% gunion(list(lrr, lfr)) %>>% po("unbranch"))
  expect_equal(lrn$predict_type, "response")
  lrn = GraphLearner$new(po("branch", 2) %>>% gunion(list(lrp, lfr)) %>>% po("unbranch"))
  expect_equal(lrn$predict_type, "response")

  # with additional NOP in branch
  lrn = GraphLearner$new(po("branch", 2) %>>% gunion(list(lrp %>>% nop, lfp)) %>>% po("unbranch"))
  expect_equal(lrn$predict_type, "prob")

  # Regression
  lrrp = po(lrn("regr.featureless", predict_type = "se"))
  lrrr = po(lrn("regr.rpart"))
  lrn = GraphLearner$new(pipeline_greplicate(po("subsample") %>>% lrrr, 3L) %>>% po("regravg"))
  expect_equal(lrn$predict_type, "response")
  lrn = GraphLearner$new(pipeline_greplicate(po("subsample") %>>% lrrp, 3L) %>>% po("regravg"))
  expect_equal(lrn$predict_type, "se")

  lrn = GraphLearner$new(lrrp %>>% nop)
  expect_equal(lrn$predict_type, "se")


  # Setter:
  lrp = po(lrn("classif.rpart", predict_type = "prob"))

  lrn = GraphLearner$new(lrp)
  lrn$predict_type = "prob"
  expect_equal(lrn$predict_type, "prob")
  expect_equal(lrn$graph$pipeops[[lrp$id]]$predict_type, "prob")

  lrn = GraphLearner$new(lrp)
  lrn$predict_type = "response"
  expect_equal(lrn$predict_type, "response")
  expect_equal(lrn$graph$pipeops[[lrp$id]]$predict_type, "response")

  lrn = GraphLearner$new(lrp)
  lrn$predict_type = "prob"
  expect_equal(lrn$predict_type, "prob")
  expect_equal(lrn$graph$pipeops[[lrp$id]]$predict_type, "prob")

  lrn = GraphLearner$new(lrp %>>% po("nop"))
  lrn$predict_type = "response"
  expect_equal(lrn$predict_type, "response")
  expect_equal(lrn$graph$pipeops[[lrp$id]]$predict_type, "response")

  # averager
  lrn = GraphLearner$new(pipeline_greplicate(po("subsample") %>>% lrp %>>% nop, 3L) %>>% po("classifavg"))
  lrn$predict_type = "response"
  expect_equal(lrn$predict_type, "response")
  expect_true(all(map_chr(lrn$graph$pipeops[paste(lrp$id, 1:3, sep = "_")], "predict_type") == "response"))

  # branching
  lrn = GraphLearner$new(po("branch", 2) %>>% gunion(list(lrp, lfp %>>% nop)) %>>% po("unbranch"))
  expect_equal(lrn$predict_type, "prob")
  lrn$predict_type = "response"
  expect_equal(lrn$predict_type, "response")
  expect_equal(lrn$graph$pipeops[[lrp$id]]$predict_type, "response")
  expect_equal(lrn$graph$pipeops[[lfp$id]]$predict_type, "response")

  # Setter on construction
  lrn = GraphLearner$new(lrr, predict_type = "prob")
  expect_equal(lrr$predict_type, "response")
  expect_equal(lrn$predict_type, "prob")
  expect_equal(lrn$graph$pipeops[[lrr$id]]$predict_type, "prob")

  # Errors:
  expect_error({lrrp = po(lrn("classif.featureless", predict_type = "se"))})
})


test_that("GraphLearner model", {
  skip_if_not_installed("rpart")
  graph = po("pca") %>>% lrn("classif.rpart")
  graph2 = graph$clone(deep = TRUE)
  graph_orig = graph$clone(deep = TRUE)

  lr = GraphLearner$new(graph)

  expect_equal(lr$graph, graph)
  expect_equal(lr$graph_model, graph)

  graph2$train(tsk("iris"))

  lr$train(tsk("iris"))

  expect_equal(graph, graph_orig)
  expect_null(graph$state$pca)

  # behind-the-scenes param_set cache ruins expect_equal if we don't do this:
  graph_orig$param_set

  expect_equal(lr$graph, graph_orig)

  graph2$state$classif.rpart$train_time = 0
  lr$state$model$classif.rpart$train_time = 0

  expect_equal(lr$graph_model, graph2)

  imp = graph2$pipeops$classif.rpart$learner_model$importance()

  expect_equal(lr$graph_model$pipeops$classif.rpart$learner_model$importance(), imp)


})

test_that("predict() function for Graph", {
  skip_if_not_installed("rpart")

  lx = as_graph(lrn("classif.rpart"))

  lx$train(tsk("iris"))

  p1 = lx$pipeops$classif.rpart$learner_model$predict(tsk("iris"))

  expect_equal(predict(lx, tsk("iris")), p1)

  expect_error(predict(lx, iris[1:4]), "Could not create a classif-task for plain prediction data")

  lx = as_graph(lrn("regr.rpart"))

  lx$train(tsk("boston_housing_classic"))

  p1 = lx$pipeops$regr.rpart$learner_model$predict(tsk("boston_housing_classic"))

  expect_equal(predict(lx, tsk("boston_housing_classic")), p1)

  expect_equal(
    predict(lx, tsk("boston_housing_classic")$data(cols = tsk("boston_housing_classic")$feature_names)),
    p1$response
  )


})

test_that("base_learner() works", {
  skip_if_not_installed("rpart")
  # graph containing single PipeOpLearner
  x = as_learner(as_graph(lrn("classif.rpart")))
  # untrained
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)
  # trained:
  x$train(tsk("iris"))
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)

  # graph consisting of PCA, rpart, threshold
  x = as_learner(po("pca") %>>% lrn("classif.rpart") %>>% po("threshold"))
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)
  # trained:
  x$train(tsk("iris"))
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)

  # graph inside a graph
  x = as_learner(po("pca") %>>% as_learner(po("scale") %>>% lrn("classif.rpart")) %>>% po("threshold"))
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$graph_model$pipeops$scale.classif.rpart$learner_model)
  expect_identical(x$base_learner(2), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$scale.classif.rpart$learner_model$graph_model$pipeops$classif.rpart$learner_model)
  x$train(tsk("iris"))
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$graph_model$pipeops$scale.classif.rpart$learner_model)
  expect_identical(x$base_learner(2), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$scale.classif.rpart$learner_model$graph_model$pipeops$classif.rpart$learner_model)

  # branching: now supported
  branching_learner = as_learner(ppl("branch", lrns(c("classif.rpart", "classif.debug"))))
  expect_identical(branching_learner$base_learner(), branching_learner$graph_model$pipeops$classif.rpart$learner_model)
  branching_learner$param_set$values$branch.selection = "classif.debug"
  expect_identical(branching_learner$base_learner(), branching_learner$graph_model$pipeops$classif.debug$learner_model)

  branching_learner = as_learner(ppl("branch", pos(c("pca", "ica")), prefix_branchops = "brunch") %>>% ppl("branch", lrns(c("classif.rpart", "classif.debug"))))
  expect_identical(branching_learner$base_learner(), branching_learner$graph_model$pipeops$classif.rpart$learner_model)
  branching_learner$param_set$values$branch.selection = "classif.debug"
  expect_identical(branching_learner$base_learner(), branching_learner$graph_model$pipeops$classif.debug$learner_model)

  # unbranch with single input, without corresponding PipeOpBranch, is legal
  x = as_learner(po("pca") %>>% lrn("classif.rpart") %>>% po("unbranch", 1))
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)

  # ParamInt selection parameter
  x = as_learner(ppl("branch", list(lrn("classif.rpart") %>>% po("unbranch", 1, id = "poub1"), lrn("classif.debug") %>>% po("unbranch", 1, id = "poub2"))))
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)
  x$param_set$values$branch.selection = 2
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.debug$learner_model)

  x$param_set$values$branch.selection = to_tune()
  expect_error(x$base_learner(), "Cannot infer active output.* PipeOpBranch branch.*non-numeric 'selection'")

  x = as_learner(ppl("branch", list(classif.rpart = lrn("classif.rpart") %>>% po("unbranch", 1, id = "poub1"), classif.debug = lrn("classif.debug") %>>% po("unbranch", 1, id = "poub2"))))
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)
  x$param_set$values$branch.selection = "classif.debug"
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.debug$learner_model)

  expect_error(x$base_learner(return_po = TRUE), "recursive must be == 1 if return_po is TRUE")
  expect_error(x$base_learner(return_po = TRUE, recursive = 0), "recursive must be == 1 if return_po is TRUE")
  expect_error(x$base_learner(return_all = TRUE), "recursive must be <= 1 if return_all is TRUE")

  expect_identical(x$base_learner(recursive = 1, return_po = TRUE), x$graph_model$pipeops$classif.debug)

  expect_identical(x$base_learner(recursive = 1, return_all = TRUE),
    list(x$graph_model$pipeops$classif.debug$learner_model))

  expect_identical(x$base_learner(recursive = 1, return_po = TRUE, return_all = TRUE),
    list(x$graph_model$pipeops$classif.debug))

  expect_identical(x$base_learner(recursive = 1, return_po = TRUE, return_all = TRUE, resolve_branching = FALSE),
    list(x$graph_model$pipeops$classif.rpart, x$graph_model$pipeops$classif.debug))

  expect_identical(x$base_learner(recursive = 0), x)
  expect_identical(x$base_learner(recursive = 0, return_all = TRUE), list(x))

  # unbranch as very first operation works
  x = as_learner(po("unbranch", 1, id = "dummyub1") %>>% ppl("branch", list(classif.rpart = lrn("classif.rpart") %>>% po("unbranch", 1, id = "poub1"), classif.debug = lrn("classif.debug") %>>% po("unbranch", 1, id = "poub2"))))
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)
  x$param_set$values$branch.selection = "classif.debug"
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.debug$learner_model)

  # branch with output connected to several pipeops
  x = as_learner(ppl("branch", list(classif.rpart = list(po("nop"), po("nop_1")) %>>% po("featureunion") %>>% lrn("classif.rpart") %>>% po("unbranch", 1, id = "poub1"), classif.debug = lrn("classif.debug") %>>% po("unbranch", 1, id = "poub2"))))
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)
  x$param_set$values$branch.selection = "classif.debug"
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.debug$learner_model)

  x = as_learner(ppl("branch", list(classif.rpart = po("featureunion", 2) %>>% lrn("classif.rpart") %>>% po("unbranch", 1, id = "poub1"), classif.debug = lrn("classif.debug") %>>% po("unbranch", 1, id = "poub2"))))
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)
  x$param_set$values$branch.selection = "classif.debug"
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.debug$learner_model)


  x = as_learner(po("unbranch", 2, id = "dummyub1") %>>% ppl("branch", list(classif.rpart = lrn("classif.rpart") %>>% po("unbranch", 1, id = "poub1"), classif.debug = lrn("classif.debug") %>>% po("unbranch", 1, id = "poub2"))))
  expect_error(x$base_learner(), "dummyub1 has multiple active inputs.*'input1'.*direct Graph input.*'input2'.*direct Graph input")

  ################################################################################
  ### The following graphs are bogus for multiple reasons. If our graph checks ###
  ### ever become more rigorous, these will have to be adjusted.               ###
  ################################################################################

  # pipeopbranch connected to unbranch through the same output in multiple routes.
  g = (po("branch", c("classif.rpart", "classif.debug")) %>>% list(lrn("classif.rpart"), lrn("classif.debug")))$
    add_pipeop(po("unbranch", 3))$
    add_edge("classif.rpart", "unbranch", dst_channel = "input1")$
    add_edge("classif.debug", "unbranch", dst_channel = "input2")$
    add_edge("branch", "unbranch", src_channel = "classif.debug", dst_channel = "input3")

  # classif.rpart route has only one connection --> works
  expect_identical(as_learner(g)$base_learner(), g$pipeops$classif.rpart$learner_model)

  g$param_set$values$branch.selection = "classif.debug"
  expect_error(as_learner(g)$base_learner(), "PipeOpUnbranch unbranch.*multiple active inputs.*'input2'.*PipeOpBranch 'branch'.*'classif.debug'.*'input3'.*PipeOpBranch 'branch'.*'classif.debug'")

  # live input from graph input is recognized correctly
  g = (po("branch", c("classif.rpart", "classif.debug")) %>>% list(lrn("classif.rpart"), lrn("classif.debug")))$
    add_pipeop(po("unbranch", 2, id = "poub1"))$
    add_pipeop(po("unbranch", 1, id = "poub2"))$
    add_pipeop(po("unbranch", 2))$
    add_edge("classif.rpart", "poub1", dst_channel = "input1")$
    add_edge("classif.debug", "poub2")$
    add_edge("poub1", "unbranch", dst_channel = "input1")$
    add_edge("poub2", "unbranch", dst_channel = "input2")

  # error because poub1 gets two live inputs: from PipeOpBranch and from graph input
  expect_error(as_learner(g)$base_learner(),
    "poub1 has multiple active inputs.*'input1'.*active output 'classif.rpart'.*'input2'.*direct Graph input")

  g$param_set$values$branch.selection = "classif.debug"

  # now poub1 has only one live input, since PipeOpBranch's active output goes to classif.debug / poub2;
  # however, the overall unbranch now has two active inputs
  expect_error(as_learner(g)$base_learner(),
    "PipeOpUnbranch unbranch has multiple active inputs.*'input1'.* from direct Graph input.* always active.*'input2'.* 'branch' active output 'classif.debug'")

  g = Graph$new()$
    add_pipeop(po("branch", c("classif.rpart", "classif.debug"), id = "branch1"))$
    add_pipeop(po("branch", c("classif.rpart", "classif.debug"), id = "branch2"))$
    add_pipeop(po("featureunion", id = "fu1"))$
    add_pipeop(po("featureunion", id = "fu2"))$
    add_pipeop(lrn("classif.rpart"))$
    add_pipeop(lrn("classif.debug"))$
    add_pipeop(po("unbranch", 2))$
    add_edge("branch1", "fu1", src_channel = "classif.rpart")$
    add_edge("branch2", "fu1", src_channel = "classif.rpart")$
    add_edge("branch1", "fu2", src_channel = "classif.debug")$
    add_edge("branch2", "fu2", src_channel = "classif.debug")$
    add_edge("fu1", "classif.rpart")$
    add_edge("fu2", "classif.debug")$
    add_edge("classif.rpart", "unbranch", dst_channel = "input1")$
    add_edge("classif.debug", "unbranch", dst_channel = "input2")

  # this works as long as branch1 and branch2 flow in the same direction
  expect_identical(as_learner(g)$base_learner(), g$pipeops$classif.rpart$learner_model)

  g$param_set$set_values(branch1.selection = "classif.debug", branch2.selection = "classif.debug")
  expect_identical(as_learner(g)$base_learner(), g$pipeops$classif.debug$learner_model)

  g$param_set$set_values(branch1.selection = "classif.rpart", branch2.selection = "classif.debug")
  ## there are multiple possible error messages here: either at fu1, or at fu2.
  ## currently we find fu1 first because this is what the topological sort results in (fu1 gets checked first),
  ## but an error at fu2 would be equally valid.
  expect_error(as_learner(g)$base_learner(),
    "'branch2' inactive output.*'classif.rpart'.*'branch1' active output.*'classif.rpart'.*at PipeOp fu1|'branch1' inactive output.*'classif.debug'.*'branch2' active output.*'classif.debug'.*at PipeOp fu2")

  g$add_pipeop(po("nop"))$add_edge("nop", "fu2")

  g$param_set$set_values(branch1.selection = "classif.debug", branch2.selection = "classif.debug")
  expect_identical(as_learner(g)$base_learner(), g$pipeops$classif.debug$learner_model)

  g$param_set$set_values(branch1.selection = "classif.rpart", branch2.selection = "classif.rpart")

  expect_error(as_learner(g)$base_learner(),
    "'branch1' inactive output.*classif.debug.*'branch2' inactive output.*classif.debug.* in conflict with direct Graph input.*fu2")

  # bogus GraphLearner with no PipeOpLearner inside.
  expect_error(as_learner(po("nop"))$base_learner(), "No base learner found in Graph.")


  bagger = as_learner(ppl("bagging", iterations = 1, lrn("classif.rpart"),
    averager = po("classifavg", collect_multiplicity = TRUE)))

  expect_identical(bagger$base_learner(), bagger$graph_model$pipeops$classif.rpart$learner_model)

  bagger$train(tsk("iris"))

  # ....$learner_model is a multiplicity, but base_learner() unpacks it.
  expect_identical(bagger$base_learner(), bagger$graph_model$pipeops$classif.rpart$learner_model[[1]])

  bagger$param_set$values$replicate.reps = 2

  bagger$train(tsk("iris"))

  expect_error(bagger$base_learner(), "Multiplicity that does not contain exactly one Learner")

  metabagger = as_learner(ppl("bagging", iterations = 1,
      ppl("bagging", iterations = 1, lrn("classif.rpart"),
        averager = po("classifavg_1", collect_multiplicity = TRUE))$set_names(c("replicate", "subsample"), c("replicate_1", "subsample_1")),
    averager = po("classifavg_2", collect_multiplicity = TRUE)))

  expect_identical(metabagger$base_learner(), metabagger$graph_model$pipeops$classif.rpart$learner_model)

  metabagger$train(tsk("iris"))
  expect_identical(metabagger$base_learner(), metabagger$graph_model$pipeops$classif.rpart$learner_model[[1]][[1]])

  metabagger$param_set$values$replicate.reps = 2
  metabagger$train(tsk("iris"))
  expect_error(metabagger$base_learner(), "Multiplicity that does not contain exactly one Learner")

  metabagger$param_set$values$replicate.reps = 1
  metabagger$train(tsk("iris"))
  expect_identical(metabagger$base_learner(), metabagger$graph_model$pipeops$classif.rpart$learner_model[[1]][[1]])

  metabagger$param_set$values$replicate_1.reps = 2
  metabagger$train(tsk("iris"))
  expect_error(metabagger$base_learner(), "Multiplicity that does not contain exactly one Learner")

  # double-channel pipeop cascade: see there is no exponential explosion
  gchain = chain_graphs(
    lapply(1:20, function(i) ppl("branch", pos(paste0("nop_", c(2 * i, 2 * i + 1))), prefix_branchops = as.character(i))),
    in_place = TRUE
  )

  glrn = as_learner(gchain %>>% ppl("branch", lrns(c("classif.rpart", "classif.debug"))))

  expect_identical(glrn$base_learner(), glrn$graph_model$pipeops$classif.rpart$learner_model)
  expect_identical(glrn$base_learner(recursive = 1, return_all = TRUE, resolve_branching = FALSE),
    list(glrn$graph_model$pipeops$classif.rpart$learner_model, glrn$graph_model$pipeops$classif.debug$learner_model))


  glrn = as_learner(ppl("branch", lrns(c("classif.rpart", "classif.debug"))) %>>% gchain)

  expect_identical(glrn$base_learner(), glrn$graph_model$pipeops$classif.rpart$learner_model)

  # without memoization the following would take a long time
  expect_identical(glrn$base_learner(recursive = 1, return_all = TRUE, resolve_branching = FALSE),
    list(glrn$graph_model$pipeops$classif.rpart$learner_model, glrn$graph_model$pipeops$classif.debug$learner_model))

  glrn$base_learner()


  # branch -> unbranch, unbranch -> unbranch

  g = Graph$new()$
    add_pipeop(po("branch", c("classif.rpart", "dummy1", "dummy2", "classif.debug"), id = "branch1"))$
    add_pipeop(po("unbranch", 2, id = "unbranch1"))$
    add_pipeop(po("unbranch", 2, id = "unbranch2"))$
    add_pipeop(po("unbranch", 2, id = "unbranch3"))$
    add_pipeop(lrn("classif.rpart"))$
    add_pipeop(lrn("classif.debug"))$
    add_edge("branch1", "classif.rpart", src_channel = "classif.rpart")$
    add_edge("branch1", "classif.debug", src_channel = "classif.debug")$
    add_edge("classif.rpart", "unbranch1", dst_channel = "input1")$
    add_edge("classif.debug", "unbranch2", dst_channel = "input1")$
    add_edge("unbranch1", "unbranch3", dst_channel = "input1")$
    add_edge("unbranch2", "unbranch3", dst_channel = "input2")$
    add_edge("branch1", "unbranch1", src_channel = "dummy1", dst_channel = "input2")$
    add_edge("branch1", "unbranch2", src_channel = "dummy2", dst_channel = "input2")

  expect_identical(as_learner(g)$base_learner(), g$pipeops$classif.rpart$learner_model)
  g$param_set$values$branch1.selection = "classif.debug"
  expect_identical(as_learner(g)$base_learner(), g$pipeops$classif.debug$learner_model)
  g$param_set$values$branch1.selection = "dummy1"
  expect_equal(g$train(1), list(unbranch3.output = 1))

  expect_error(as_learner(g)$base_learner(), "No base learner found")

  expect_identical(as_learner(g)$base_learner(recursive = 1, resolve_branching = FALSE, return_all = TRUE),
    list(g$pipeops$classif.rpart$learner_model, g$pipeops$classif.debug$learner_model))

  # infer task type and other things: use base_learner, but don't do that if things are given explicitly.
})


test_that("GraphLearner hashes", {
  skip_if_not_installed("rpart")


  learner1 = as_learner(ppl("robustify") %>>% lrn("regr.rpart"))
  learner1dash = as_learner(ppl("robustify") %>>% lrn("regr.rpart"))

  expect_string(learner1$hash)
  expect_string(learner1$phash)

  # to compare hashes, we need to set the function-valued hyperparameters equal;
  # otherwise, they have different environments and may hash differently.

  funparams = names(which(map_lgl(learner1$param_set$values, is.function)))
  learner1dash$param_set$set_values(.values = learner1$param_set$values[funparams])

  expect_equal(learner1$hash, learner1dash$hash)
  expect_equal(learner1$phash, learner1dash$phash)

  learner1dash$graph$pipeops$regr.rpart$param_set$values$xval = 1

  expect_string(all.equal(learner1$hash, learner1dash$hash), "mismatch")
  expect_equal(learner1$phash, learner1dash$phash)

  learner2 = as_learner(po("pca") %>>% lrn("regr.rpart"))

  expect_string(all.equal(learner1$hash, learner2$hash), "mismatch")
  expect_string(all.equal(learner1$phash, learner2$phash), "mismatch")

  learner1$id = "myid"
  learner2$id = "myid"

  expect_string(all.equal(learner1$hash, learner2$hash), "mismatch")
  expect_string(all.equal(learner1$phash, learner2$phash), "mismatch")


  # construction argument dependent hashes
  expect_string(all.equal(po("copy", 2)$hash, po("copy", 3)$hash), "mismatch")


  lr1 <- lrn("classif.rpart")
  lr2 <- lrn("classif.rpart", fallback = lrn("classif.rpart"))

  expect_string(all.equal(lr1$hash, lr2$hash), "mismatch")
  expect_string(all.equal(lr1$phash, lr2$phash), "mismatch")

  lr1 <- as_learner(as_pipeop(lr1))
  lr2 <- as_learner(as_pipeop(lr2))

  expect_string(all.equal(lr1$hash, lr2$hash), "mismatch")
  expect_string(all.equal(lr1$phash, lr2$phash), "mismatch")

  lr1 <- as_learner(as_pipeop(lr1))
  lr2 <- as_learner(as_pipeop(lr2))

  expect_string(all.equal(lr1$hash, lr2$hash), "mismatch")
  expect_string(all.equal(lr1$phash, lr2$phash), "mismatch")

})

test_that("validation, internal_valid_scores", {
  expect_error(as_pipeop(lrn("classif.debug", validate = 0.3)), "must either be")
  # None of the Learners can do validation -> NULL
  glrn1 = as_learner(as_graph(lrn("classif.rpart")))$train(tsk("iris"))
  expect_false("validation" %in% glrn1$properties)
  expect_equal(glrn1$internal_valid_scores, NULL)

  glrn2 = as_learner(as_graph(lrn("classif.debug")))
  expect_true("validation" %in% glrn2$properties)
  set_validate(glrn2, 0.2)
  expect_equal(glrn2$validate, 0.2)
  expect_equal(glrn2$graph$pipeops$classif.debug$learner$validate, "predefined")
  glrn2$train(tsk("iris"))
  expect_list(glrn2$internal_valid_scores, types = "numeric")
  expect_equal(names(glrn2$internal_valid_scores), "classif.debug.acc")

  set_validate(glrn2, NULL)
  glrn2$train(tsk("iris"))
  expect_true(is.null(glrn2$internal_valid_scores))

  # No validation set specified --> No internal_valid_scores
  expect_equal(
    as_learner(as_graph(lrn("classif.debug")))$train(tsk("iris"))$internal_valid_scores,
    NULL
  )
  glrn2 = as_learner(as_graph(lrn("classif.debug")))
})

test_that("internal_tuned_values", {
  # no internal tuning support -> NULL
  task = tsk("iris")
  glrn1 = as_learner(as_graph(lrn("classif.rpart")))$train(task)
  expect_false("internal_tuning" %in% glrn1$properties)
  expect_equal(glrn1$internal_tuned_values, NULL)

  # learner wQ
  # ith internal tuning
  glrn2 = as_learner(as_graph(lrn("classif.debug")))
  expect_true("internal_tuning" %in% glrn2$properties)
  expect_equal(glrn2$internal_tuned_values, NULL)
  glrn2$train(task)
  expect_equal(glrn2$internal_tuned_values, named_list())
  glrn2$param_set$set_values(classif.debug.early_stopping = TRUE, classif.debug.iter = 1000)
  set_validate(glrn2, 0.2)
  glrn2$train(task)
  expect_equal(names(glrn2$internal_tuned_values), "classif.debug.iter")
})

test_that("set_validate", {
  glrn = as_learner(as_pipeop(lrn("classif.debug", validate = "predefined")))
  set_validate(glrn, "test")
  expect_equal(glrn$validate, "test")
  expect_equal(glrn$graph$pipeops$classif.debug$learner$validate, "predefined")
  set_validate(glrn, NULL)
  expect_equal(glrn$validate, NULL)
  expect_equal(glrn$graph$pipeops$classif.debug$learner$validate, NULL)
  set_validate(glrn, 0.2, ids = "classif.debug")
  expect_equal(glrn$validate, 0.2)
  expect_equal(glrn$graph$pipeops$classif.debug$learner$validate, "predefined")

  glrn = as_learner(ppl("branch", list(lrn("classif.debug"), lrn("classif.debug", id = "final"), lrn("classif.featureless"))))
  set_validate(glrn, 0.3, ids = c("classif.debug", "final"))
  expect_equal(glrn$validate, 0.3)
  expect_equal(glrn$graph$pipeops$classif.debug$learner$validate, "predefined")
  expect_equal(glrn$graph$pipeops$final$learner$validate, "predefined")
  expect_error(set_validate(glrn, 0.2, ids = "classif.featureless"))


  glrn = as_learner(ppl("stacking", list(lrn("classif.debug"), lrn("classif.featureless")),
    lrn("classif.debug", id = "final")))
  glrn2 = as_learner(po("learner", glrn, id = "polearner"))
  set_validate(glrn2, validate = 0.25, ids = "polearner", args = list(polearner = list(ids = "final")))
  expect_equal(glrn2$validate, 0.25)
  expect_equal(glrn2$graph$pipeops$polearner$learner$validate, "predefined")
  expect_equal(glrn2$graph$pipeops$polearner$learner$graph$pipeops$final$learner$validate, "predefined")
  expect_equal(glrn2$graph$pipeops$polearner$learner$graph$pipeops$classif.debug$learner$validate, NULL)

  # graphlearner in graphlearner: failure handling
  glrn = as_learner(po("pca") %>>% lrn("classif.debug"))
  po_glrn = as_pipeop(glrn)
  po_glrn$id = "po_glrn"
  gglrn = as_learner(po_glrn)
  expect_error(
    set_validate(gglrn, validate = "test", args = list(po_glrn = list(ids = "pca"))),
    "Trying to heuristically reset"
  )
  expect_equal(gglrn$validate, NULL)

  # base_learner is not final learner
  glrn = as_learner(lrn("classif.debug") %>>% po("nop"))
  set_validate(glrn, 0.3)
  expect_equal(glrn$graph$pipeops$classif.debug$validate, "predefined")
  set_validate(glrn, NULL)
  expect_equal(glrn$graph$pipeops$classif.debug$validate, NULL)
  expect_equal(glrn$validate, NULL)

  # args and args_all
  bglrn = as_learner(ppl("branch", list(lrn("classif.debug", id = "d1"), lrn("classif.debug", id = "d2"))))

  obj = as_pipeop(bglrn)
  obj$id = "po_glrn"
  gglrn = as_learner(obj)

  # args
  set_validate(gglrn, validate = 0.2, args = list(po_glrn = list(ids = "d1")))
  expect_equal(gglrn$graph$pipeops[[1L]]$learner$graph$pipeops$d1$validate, "predefined")
  expect_equal(gglrn$graph$pipeops[[1L]]$learner$graph$pipeops$d2$validate, NULL)

  # args all
  gglrn = as_learner(obj)
  set_validate(gglrn, validate = 0.2, args_all = list(ids = "d1"))
  expect_equal(gglrn$graph$pipeops[[1L]]$learner$graph$pipeops$d1$validate, "predefined")
  expect_equal(gglrn$graph$pipeops[[1L]]$learner$graph$pipeops$d2$validate, NULL)
})

test_that("marshal", {
  task = tsk("iris")
  glrn = as_learner(as_graph(lrn("classif.debug")))
  glrn$train(task)
  p1 = glrn$predict(task)
  glrn$marshal()
  expect_true(glrn$marshaled)
  expect_true(is_marshaled_model(glrn$state$model$marshaled$classif.debug))
  glrn$unmarshal()
  expect_false(is_marshaled_model(glrn$state$model$marshaled$classif.debug))
  expect_class(glrn$model, "graph_learner_model")
  expect_false(is_marshaled_model(glrn$state$model$marshaled$classif.debug$model))

  p2 = glrn$predict(task)
  expect_equal(p1$response, p2$response)

  # checks that it is marshalable
  glrn$train(task)
  expect_learner(glrn, task)
})

test_that("marshal has no effect when nothing needed marshaling", {
  task = tsk("iris")
  glrn = as_learner(as_graph(lrn("classif.rpart")))
  glrn$train(task)
  glrn$marshal()
  expect_class(glrn$marshal()$model, "graph_learner_model")
  expect_class(glrn$unmarshal()$model, "graph_learner_model")
  expect_learner(glrn, task = task)
})


# in case Debug ever gets these properties, we remove them here.
DebugBasic = R6Class("DebugBasic", inherit = LearnerClassifDebug,
  public = list(
    initialize = function(...) {
      super$initialize(...)
      self$properties = setdiff(self$properties, c("importance", "selected_features", "loglik", "oob_error"))
    },
    importance = function() {
      return(sapply(self$state$feature_names, function(i) 1))
    }
))

test_that("GraphLearner Importance", {

  DebugWithImportance = R6Class("DebugWithImportance", inherit = LearnerClassifDebug,
    public = list(
      initialize = function(...) {
        super$initialize(...)
        self$properties = c(setdiff(self$properties, c("importance", "selected_features", "loglik", "oob_error")), "importance")
      },
      importance = function() {
        return(sapply(self$state$feature_names, function(i) 1))
      }
  ))

  g_basic = GraphLearner$new(Graph$new()$add_pipeop(DebugBasic$new()))
  g_importance = GraphLearner$new(Graph$new()$add_pipeop(DebugWithImportance$new()))

  expect_false("importance" %in% g_basic$properties)
  expect_true("importance" %in% g_importance$properties)
  expect_false(any(c("loglik", "oob_error") %in% g_basic$properties))
  expect_false(any(c("loglik", "oob_error") %in% g_importance$properties))

  expect_error(g_basic$importance(), "does not implement.*importance")

  g_importance$train(tsk("iris"))

  expect_equal(g_importance$importance(), c(Petal.Length = 1, Petal.Width = 1, Sepal.Length = 1, Sepal.Width = 1))

  g_bagging = as_learner(ppl("bagging", DebugWithImportance$new(), averager = po("classifavg", collect_multiplicity = TRUE)))

  expect_true("importance" %in% g_bagging$properties)
  g_bagging$train(tsk("iris"))
  expect_true("importance" %in% g_bagging$properties)
  expect_error(g_bagging$importance(), "Multiplicity that does not contain exactly one Learner")

  g_bagging$param_set$values$replicate.reps = 1
  g_bagging$train(tsk("iris"))
  expect_equal(g_bagging$importance(), c(Petal.Length = 1, Petal.Width = 1, Sepal.Length = 1, Sepal.Width = 1))

  # * test importance with feature selection
  g_fs = as_learner(po("select", selector = selector_grep("Sepal")) %>>% DebugWithImportance$new())
  g_fs$train(tsk("iris"))
  expect_equal(g_fs$importance(), c(Sepal.Length = 1, Sepal.Width = 1))

  # branching
  lbasic = DebugBasic$new()
  lbasic$id = "basic"
  g_branch = as_learner(ppl("branch", list(basic = lbasic, importance = DebugWithImportance$new())))

  expect_true("importance" %in% g_branch$properties)
  g_branch$train(tsk("iris"))
  expect_error(g_branch$importance(), "does not implement.*importance")
  g_branch$param_set$values$branch.selection = "importance"
  g_branch$train(tsk("iris"))
  expect_equal(g_branch$importance(), c(Petal.Length = 1, Petal.Width = 1, Sepal.Length = 1, Sepal.Width = 1))

  g_nested = as_learner(as_graph(as_learner(as_graph(DebugWithImportance$new()))))
  expect_equal(
    g_nested$train(tsk("iris"))$importance(),
    c(Petal.Length = 1, Petal.Width = 1, Sepal.Length = 1, Sepal.Width = 1)
  )

  expect_true("importance" %in% g_nested$properties)
  g_nested_basic = as_learner(as_graph(as_learner(as_graph(DebugBasic$new()))))
  expect_false("importance" %in% g_nested_basic$properties)

  expect_false(any(c("loglik", "oob_error") %in% g_nested_basic$properties))
  expect_false(any(c("loglik", "oob_error") %in% g_nested$properties))
})


test_that("GraphLearner Selected Features", {

  DebugWithSelectedFeatures = R6Class("DebugWithFeatsel", inherit = LearnerClassifDebug,
    public = list(
      initialize = function(...) {
        super$initialize(...)
        self$properties = c(setdiff(self$properties, c("importance", "selected_features", "loglik", "oob_error")), "selected_features")
      },
      selected_features = function() {
        return(self$state$feature_names[[1]])
      }
  ))

  g_basic = GraphLearner$new(Graph$new()$add_pipeop(DebugBasic$new()))
  g_featsel = GraphLearner$new(Graph$new()$add_pipeop(DebugWithSelectedFeatures$new()))

  expect_true("selected_features" %in% g_basic$properties)
  expect_true("selected_features" %in% g_featsel$properties)
  expect_false(any(c("importance", "loglik", "oob_error") %in% g_featsel$properties))
  expect_false(any(c("importance", "loglik", "oob_error") %in% g_featsel$properties))

  expect_error(g_basic$selected_features(), "does not implement.*selected_features.*impute_selected_features.*TRUE")
  g_basic$impute_selected_features = TRUE
  expect_error(g_basic$selected_features(), "No model stored.*classif.debug.*Graph.*classif.debug")


  g_basic$train(tsk("iris"))

  expect_equal(g_basic$selected_features(), tsk("iris")$feature_names)

  g_featsel$train(tsk("iris"))

  expect_equal(g_featsel$selected_features(), tsk("iris")$feature_names[[1]])

  # does not accidentally impute if it doesn't need to
  g_featsel$impute_selected_features = TRUE
  expect_equal(g_featsel$selected_features(), tsk("iris")$feature_names[[1]])

  g_bagging = as_learner(ppl("bagging", DebugWithSelectedFeatures$new(), averager = po("classifavg", collect_multiplicity = TRUE)))

  expect_true("selected_features" %in% g_bagging$properties)
  g_bagging$train(tsk("iris"))
  expect_true("selected_features" %in% g_bagging$properties)
  expect_equal(g_bagging$selected_features(), tsk("iris")$feature_names[[1]])


  g_fs = as_learner(po("select", selector = selector_grep("Sepal")) %>>% DebugWithSelectedFeatures$new())
  g_fs$train(tsk("iris"))
  expect_equal(g_fs$selected_features(), "Sepal.Length")

  g_fs_basic = as_learner(po("select", selector = selector_grep("Sepal")) %>>% DebugBasic$new())
  g_fs_basic$train(tsk("iris"))
  g_fs_basic$impute_selected_features = TRUE
  expect_equal(g_fs_basic$selected_features(), c("Sepal.Length", "Sepal.Width"))


  # branching
  lbasic = DebugBasic$new()
  lbasic$id = "basic"
  g_branch = as_learner(ppl("branch", list(basic = lbasic, fs = DebugWithSelectedFeatures$new(), featureless = lrn("classif.featureless"))))

  expect_true("selected_features" %in% g_branch$properties)
  g_branch$train(tsk("iris"))
  expect_error(g_branch$selected_features(), "does not implement.*selected_features.*impute_selected_features.*TRUE")
  g_branch$impute_selected_features = TRUE
  expect_equal(g_branch$selected_features(), tsk("iris")$feature_names)
  g_branch$param_set$values$branch.selection = "fs"
  g_branch$train(tsk("iris"))
  expect_equal(g_branch$selected_features(), tsk("iris")$feature_names[[1]])

  g_branch$param_set$values$branch.selection = "featureless"
  g_branch$train(tsk("iris"))
  expect_equal(g_branch$selected_features(), character(0))

  g_nested = as_learner(as_graph(as_learner(as_graph(DebugWithSelectedFeatures$new()))))
  expect_equal(
    g_nested$train(tsk("iris"))$selected_features(),
    tsk("iris")$feature_names[[1]]
  )

  g_nested = as_learner(as_graph(as_learner(as_graph(DebugBasic$new()))))
  expect_error(g_nested$train(tsk("iris"))$selected_features(), " classif\\.debug .*selected_features.*impute_selected_features to TRUE")
  # need to turn on imputation in the innermost graph
  g_nested$graph$pipeops$classif.debug$learner$impute_selected_features = TRUE
  expect_equal(
    g_nested$train(tsk("iris"))$selected_features(),
    tsk("iris")$feature_names
  )

  # feature union unions selected features

  g_avg = as_learner(list(po("select", selector = selector_grep("Sepal")) %>>% lbasic, DebugWithSelectedFeatures$new(), lrn("classif.featureless")) %>>% po("classifavg", 3))

  g_avg$train(tsk("iris"))
  expect_error(g_avg$selected_features(), "does not implement.*selected_features.*impute_selected_features.*TRUE")
  g_avg$impute_selected_features = TRUE

  # Sepal.* from lbasic, Petal.Length from DebugWithSelectedFeatures, nothing from featureless
  expect_equal(g_avg$selected_features(), c("Sepal.Length", "Sepal.Width", "Petal.Length"))
})



test_that("GraphLearner other properties", {

  DebugWithProperties = R6Class("DebugWithProperties", inherit = LearnerClassifDebug,
    public = list(
      initialize = function(...) {
        super$initialize(...)
        self$properties = c(
          setdiff(self$properties, c("importance", "selected_features", "loglik", "oob_error")),
          "loglik", "oob_error")
      },
      loglik = function() {
        1
      },
      oob_error = function() {
        2
      }
  ))

  g_basic = GraphLearner$new(Graph$new()$add_pipeop(DebugBasic$new()))
  g_properties = GraphLearner$new(Graph$new()$add_pipeop(DebugWithProperties$new()))

  expect_false(any(c("loglik", "oob_error") %in% g_basic$properties))
  expect_true(all(c("loglik", "oob_error") %in% g_properties$properties))

  expect_error(g_basic$loglik(), "does not implement.*loglik")
  expect_error(g_basic$oob_error(), "does not implement.*oob_error")

  g_properties$train(tsk("iris"))
  expect_equal(g_properties$loglik(), 1)
  expect_equal(g_properties$oob_error(), 2)

  g_bagging = as_learner(ppl("bagging", DebugWithProperties$new(), averager = po("classifavg", collect_multiplicity = TRUE)))

  expect_true(all(c("loglik", "oob_error") %in% g_bagging$properties))
  g_bagging$train(tsk("iris"))
  expect_true(all(c("loglik", "oob_error") %in% g_bagging$properties))
  expect_error(g_bagging$loglik(), "Multiplicity that does not contain exactly one Learner")
  expect_error(g_bagging$oob_error(), "Multiplicity that does not contain exactly one Learner")

  g_bagging$param_set$values$replicate.reps = 1
  g_bagging$train(tsk("iris"))
  expect_equal(g_bagging$loglik(), 1)
  expect_equal(g_bagging$oob_error(), 2)

  # branching
  lbasic = DebugBasic$new()
  lbasic$id = "basic"
  g_branch = as_learner(ppl("branch", list(basic = lbasic, properties = DebugWithProperties$new())))

  expect_true(all(c("loglik", "oob_error") %in% g_branch$properties))
  g_branch$train(tsk("iris"))
  expect_error(g_branch$loglik(), "does not implement.*loglik")
  expect_error(g_branch$oob_error(), "does not implement.*oob_error")
  g_branch$param_set$values$branch.selection = "properties"
  g_branch$train(tsk("iris"))
  expect_equal(g_branch$loglik(), 1)
  expect_equal(g_branch$oob_error(), 2)

  g_nested = as_learner(as_graph(as_learner(as_graph(DebugWithProperties$new()))))
  expect_equal(
    g_nested$train(tsk("iris"))$loglik(),
    1
  )
  expect_equal(
    g_nested$train(tsk("iris"))$oob_error(),
    2
  )

  expect_true(all(c("loglik", "oob_error") %in% g_nested$properties))

})
