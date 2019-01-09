lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_pipeop = function(po) {
  label = sprintf("pipeop '%s'", po$id)
  expect_class(po, "PipeOp", label = label)
  expect_string(po$id, label = label)
  expect_class(po$param_set, "ParamSet", label = label)
  expect_list(po$param_vals, names = "unique", label = label)
  expect_output(print(po), "PipeOp:", label = label)
  expect_character(po$packages, any.missing = FALSE, unique = TRUE, label = label)
  expect_function(po$train, args = "inputs")
  expect_function(po$predict, args = "inputs")
  expect_data_table(po$input, any.missing = FALSE)
  expect_names(names(po$input), permutation.of = c("name", "train", "predict"))
  expect_data_table(po$output, any.missing = FALSE)
  expect_names(names(po$output), permutation.of = c("name", "train", "predict"))

  # expect_null(po$state)
  # expect_null(po$result)
  # expect_character(po$train_intypes)
  # expect_character(po$train_outtypes)
  # expect_character(po$predict_intypes)
  # expect_character(po$predict_outtypes)
}

train_pipeop = function(po, inputs) {
  label = sprintf("pipeop '%s'", po$id)
  expect_pipeop(po)
  expect_null(po$state, label = label)
  expect_false(po$is_trained, label = label)
  result = po$train(inputs)
  expect_list(result, label = label)
  expect_true(!is.null(po$state), label = label)
  expect_true(po$is_trained, label = label)
  return(result)
}

predict_pipeop = function(po, inputs) {
  expect_pipeop(po)
  expect_true(po$is_trained)
  result = po$predict(inputs)
  expect_list(result)
  return(result)
}

expect_graph = function(g, n_nodes = NULL, n_edges = NULL) {
  expect_class(g, "Graph")
  expect_list(g$pipeops, "PipeOp")
  if (!is.null(n_nodes))
    expect_length(g$pipeops, n_nodes)
  expect_character(g$packages, any.missing = FALSE, unique = TRUE)

  expect_data_table(g$edges, any.missing = FALSE)
  if (!is.null(n_edges))
    expect_equal(nrow(g$edges), n_edges)


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
