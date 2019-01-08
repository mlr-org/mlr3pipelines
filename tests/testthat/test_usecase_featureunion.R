context("Usecase - Featureunion")
test_that("Usecase - Featureunion", {

  # Define PipeOp's
  op1 = PipeOpScale$new()
  scatter = PipeOpCopy$new(2)
  op2a = PipeOpPCA$new()
  op2b = PipeOpNULL$new()
  op3 = PipeOpFeatureUnion$new(2)

  # mlr3 Objects
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")
  op4 = PipeOpLearner$new(learner = lrn)

  # Construct graph
  graph = op1 %>>% scatter %>>% gunion(op2a, op2b) %>>% op3 %>>% op4
  expect_false(graph$is_trained)
  test_basic_graph_props(graph)
  expect_true(length(graph) == 6L)

  # Train and predict
  graph$train(task)
  expect_class(graph[["classif.rpart"]]$pipeop$state, "Experiment")
  # graph$predict(task)
})

