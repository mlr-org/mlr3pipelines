########## Basic operator %>>% for 1-to-1, 1-to-n, n-to-1 ##########


# FIXME: getter for length of channels
# FIXME: lhs and rhs must be the number of pipeops,  and for each pair the number of channels must be equal

# we nneed to DOCUMENT semantics of the >> op, wrt to what happens with multiple rhs / lhs channels

# we need to simply document a few cases where >> works. it does not need to in totally arbitrary cases, but robustly in the cases it
# asserts

#' @export
`%>>%` = function(g1, g2) {
  g1 = ensure_graph(g1)
  g2 = ensure_graph(g2)
  g = gunion(list(g1, g2))
  # FIXME: code needs to be checked. correct? comment a bit!
  new_edges = cbind(
    rbindlist(map(g1$rhs, function(id) data.table(src_id = id, src_channel = seq_along(g1$pipeops[[id]]$train_outtypes)))),
    rbindlist(map(g2$lhs, function(id) data.table(dst_id = id, dst_channel = seq_along(g2$pipeops[[id]]$train_outtypes))))
  )

  g$edges = rbind(g$edges, new_edges)
  g
}
