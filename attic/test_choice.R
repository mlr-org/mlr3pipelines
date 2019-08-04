

test_that("Usecase - Choice", {

  # Define PipeOps
  opchoice = PipeOpChoice$new(3)
  opchoicenamed = PipeOpChoice$new(c("opscale", "oppca", "opnop"))
  opscale = PipeOpScale$new()
  oppca = PipeOpPCA$new()
  opnop = PipeOpNOP$new()
  opunchoice = PipeOpUnchoice$new(3)

  # mlr3 Objects
  task = mlr_tasks$get("iris")

  # Construct graph
  graph1 = opchoice %>>% gunion(opscale, oppca, opnop) %>>% opunchoice
  graph2 = opchoicenamed %>>% gunion(opscale, oppca, opnop) %>>% opunchoice
  graph3 = grultiplex(opscale = opscale, .graphs = list(oppca = oppca, opnop = opnop))

  for (graph in list(graph1, graph2, graph3)) {
    expect_false(graph$is_trained)
    test_basic_graph_props(graph)
    expect_true(length(graph) == 5L)
  }

  all.equal(graph2, graph3)
  expect_equal(graph1$param_vals$choice.selection, 1)
  expect_equal(graph2$param_vals$choice.selection, "opscale")


  # Train and predict
  graph$train(task)
  expect_class(graph[["classif.rpart"]]$pipeop$state, "Learner")
  # graph$predict(task)
})
