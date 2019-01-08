expect_graph = function(g) {
  expect_class(g, "Graph")
  expect_data_table(g$channels, any.missing = FALSE)
  expect_list(g$pipeops, "PipeOp")

  expect_character(g$lhs, any.missing = FALSE)
  expect_character(g$rhs, any.missing = FALSE)
}

test_basic_graph_props = function(graph) {

  # expect_class(graph$param_set, "ParamSet")
  expect_list(graph$param_vals, names = "unique")
  expect_character(graph$packages)
  expect_list(graph$node_list, names = "unique")
  expect_list(graph$intype)
  expect_list(graph$outtype)
  expect_equivalent(as.vector(unique(sapply(graph$lhs, class))), c("GraphNode", "R6"))
  expect_equivalent(as.vector(unique(sapply(graph$rhs, class))), c("GraphNode", "R6"))

  expect_output(graph$print(), "Pipeline Graph:")
}
