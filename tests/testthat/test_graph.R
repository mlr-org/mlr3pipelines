context("Graph")

test_that("simple graph operations with stupid pipeops", {
  g = Graph$new()
  g$add(th_po_1)
})

#test_that("Graph", {
#  task = mlr_tasks$get("iris")

#  op1 = PipeOpScale$new()
#  op2 = PipeOpPCA$new()
#  lrn = mlr_learners$get("classif.rpart")
#  lrn$predict_type = "prob"

#  op3 = PipeOpLearner$new(learner = lrn)

#  root = GraphNode$new(op1)
#  root$set_next(GraphNode$new(op2))$set_next(GraphNode$new(op3))

#  g = Graph$new(root)
#  expect_class(g, "Graph")
#  #expect_output(print(g), regexp = "Graph: scaler->pca->classif.rpart")
#  expect_false(g$is_trained)

#  # Test train/predict
#  g$train(task)
#  expect_true(g$is_trained)
#  # op3$params$prediction

#  # Test active bindings
#  expect_equal(names(g$ids), c(op1$id, op2$id, op3$id))
#  expect_equal(g$param_vals, list())
#  expect_equal(g$param_set, graph_gather_params(list(root)))
#  expect_equal(g$lhs[[1]]$id, "scaler")
#  expect_equal(g$rhs$classif.rpart$id, "classif.rpart")

#  # Test `[[` operator
#  expect_class(g[["scaler"]], "GraphNode")
#  expect_equal(g[["scaler"]]$id, "scaler")
#  expect_error(g[["foo"]], "Assertion on 'id' failed:")

#  expect_equal(length(g), 3L)
#})


#test_that("Parallel graph", {
#  task = mlr_tasks$get("iris")

#  op1 = PipeOpNULL$new()
#  op2a = PipeOpScale$new()
#  op2b = PipeOpPCA$new()
#  lrn = mlr_learners$get("classif.rpart")
#  lrn$predict_type = "prob"
#  op3 = PipeOpFeatureUnion$new()
#  op4 = PipeOpLearner$new(learner = lrn)

#  root = GraphNode$new(op1)
#  root$
#    set_next(list(GraphNode$new(op2a), GraphNode$new(op2b)))$
#    set_next(GraphNode$new(op3))$
#    set_next(GraphNode$new(op4))

#  g = Graph$new(root)
#  expect_class(g, "Graph")
#  #expect_output(print(g), regexp = "Graph: OpNULL->scaler->pca->featureunion->classif.rpart")
#  expect_false(g$is_trained)

#  # Test train/predict
#  g$train(task)
#  # expect_true(g$is_trained)
#  # op3$params$prediction

#  # Test active bindings
#  expect_equal(names(g$ids), c(op1$id, op2a$id, op2b$id, op3$id, op4$id))
#  expect_equal(g$param_vals, list())
#  expect_equal(g$param_set, graph_gather_params(list(root)))
#  expect_equal(g$lhs[[1]]$id, "OpNULL")
#  expect_equal(g$rhs$classif.rpart$id, "classif.rpart")

#  # Test `[[` operator
#  expect_class(g[["scaler"]], "GraphNode")
#  expect_equal(g[["scaler"]]$id, "scaler")
#  expect_error(g[["foo"]], "Assertion on 'id' failed:")

#  expect_equal(length(g), 5L)
#})

#test_that("Graph packages", {
#  op   = PipeOpSparsePCA$new()
#  node = GraphNode$new(op)
#  g    = Graph$new(node)
#  expect_equal(g$packages, "irlba")
#})

#test_that("graph_map_topo - map function and return the output in topological order", {

#  n1 = GraphNode$new(PipeOpNULL$new("1"))
#  n2 = GraphNode$new(PipeOpNULL$new("2"))
#  n3 = GraphNode$new(PipeOpNULL$new("3"))
#  n4 = GraphNode$new(PipeOpNULL$new("4"))
#  n5 = GraphNode$new(PipeOpNULL$new("5"))
#  n6 = GraphNode$new(PipeOpNULL$new("6"))
#  n7 = GraphNode$new(PipeOpNULL$new("7"))
#  n8 = GraphNode$new(PipeOpNULL$new("8"))
#  n9 = GraphNode$new(PipeOpNULL$new("9"))
#  n10 = GraphNode$new(PipeOpNULL$new("10"))
#  n11 = GraphNode$new(PipeOpNULL$new("11"))

#  n1$set_next(list(n2, n5, n9))

#  n2$set_next(list(n3, n4))
#  n3$set_next(list(n4))

#  n5$set_next(list(n6, n7, n8))

#  n9$set_next(list(n10))
#  n10$set_next(list(n8))

#  n4$set_next(list(n11))
#  n8$set_next(list(n11))

#  # Vis the graph: graph_plot(n1)

#  expect_equivalent(
#    graph_map_topo(list(n1), function(x) x$id),
#    c("1", "2", "5", "9", "3", "6", "7", "10", "4", "8", "11")
#  )
#})
