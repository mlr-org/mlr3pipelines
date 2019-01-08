########## Basic operator %>>% for 1-to-1, 1-to-n, n-to-1 ##########

#' @export
`%>>%` = function(g1, g2) {
  UseMethod("%>>%")
}

#' @export
`%>>%.PipeOp` = function(g1, g2) {
  g1 = ensure_graph(g1)
  `%>>%`(g1, g2)
}

# FIXME: getter for length of channels
# FIXME: lhs and rhs must be the number of pipeops,  and for each pair the number of channels must be equal

#' @export
`%>>%.Graph` = function(g1, g2) {
  g2 = ensure_graph(g2)

  g = gunion(list(g1, g2))
  new_channels = cbind(
    rbindlist(map(g1$rhs, function(id) data.table(src_id = id, src_channel = seq_along(g1$pipeops[[id]]$train_outtypes)))),
    rbindlist(map(g2$lhs, function(id) data.table(dst_id = id, dst_channel = seq_along(g2$pipeops[[id]]$train_outtypes))))
  )

  g$channels = rbind(g$channels, new_channels)
  g
}
