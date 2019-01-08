lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_graph = function(g) {
  expect_class(g, "Graph")
  expect_data_table(g$channels, any.missing = FALSE)
  expect_list(g$pipeops, "PipeOp")

  expect_character(g$lhs, any.missing = FALSE)
  expect_character(g$rhs, any.missing = FALSE)
}
