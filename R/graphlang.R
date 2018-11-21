`%>>%` = function(g1, g2) {
  assert_multi_class(g1, c("Graph", "PipeOp"))
  assert_multi_class(g2, c("Graph", "PipeOp"))


  if (is(g1, "PipeOp"))
    g1 = Graph$new(GraphNode$new(g1))
  if (is(g2, "PipeOp"))
    g2 = Graph$new(GraphNode$new(g2))

  rhs = g1$rhs
  lhs = g2$lhs

  n1 = length(rhs)
  n2 = length(lhs)

  # case n-to-m
  if (n1 > 1L && n2 > 1L)
    stopf("Multiple sinks (g1 has %i) cannot be connected to multiple sources (g2 has %i)", n1, n2)

  # case 1-to-n
  if (n1 == 1L)
    rhs[[1L]]$set_next(lhs)

  # case n-to-1
  if (n2 == 1L)
    lhs[[1L]]$set_prev(rhs)

  g = Graph$new(g1$source_node)
  return(g)
}


repg = function(k, g) {
  for (i in 1:k) {

  }
}
