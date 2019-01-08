lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_graph = function(g) {
  expect_class(g, "Graph")
  expect_data_table(g$channels, any.missing = FALSE)
  expect_list(g$pipeops, "PipeOp")
  expect_character(g$packages, any.missing = FALSE, unique = TRUE)

  # expect_class(g$param_set, "ParamSet")
  # expect_list(g$param_vals, names = "unique")

  expect_character(g$lhs, any.missing = FALSE)
  expect_character(g$rhs, any.missing = FALSE)

  expect_set_equal(g$ids(), names(g$pipeops))
  expect_set_equal(g$ids(sorted = TRUE), names(g$pipeops))

  expect_flag(g$is_trained)

  # expect_list(graph$intype)
  # expect_list(graph$outtype)
}
