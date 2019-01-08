

ensure_graph = function(x) {
  assert_multi_class(x, c("Graph", "PipeOp"))
  if (inherits(x, "PipeOp"))
    x = Graph$new()$add_pipeop(x)
  return(x)
}
