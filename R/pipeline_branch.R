#' @include mlr_graphs.R

#' @title Branch Between Alternative Paths
#' @name mlr_graphs_branch
#' @description
#' Create a multiplexed graph.
#'
#' All input arguments are cloned and have no references in common with the returned [`Graph`].
#'
#' @param graphs `list` of [`Graph`] \cr
#'   Multiple graphs, possibly named. They all must have exactly
#'   one output. If any of the arguments are named, then all must have
#'   unique names.
#' @param prefix_branchops `character(1)` \cr
#'   Optional id prefix to prepend to [`PipeOpBranch`] and [`PipeOpUnbranch`] id. Their
#'   resulting IDs will be `"[prefix_branchops]branch"` and `"[prefix_branchops]unbranch"`.
#'   Default is `""`.
#' @param prefix_paths `logical(1)` | `character(1)` \cr
#'   Whether to add prefixes to graph IDs when performing gunion. Can be helpful to
#'   avoid ID clashes in resulting graph. Default `FALSE`. If this is `TRUE`, the prefixes
#'   are taken from the names of the input arguments if present or `"poX"` where X counts up. If this is
#'   a `character(1)`, it is a prefix that is added to the `PipeOp` IDs *additionally*
#'   to the input argument list.
#'
#' @return [`Graph`]
#' @export
#' @examples
#' library("mlr3")
#'
#' po_pca = po("pca")
#' po_nop = po("nop")
#'
#' branches = pipeline_branch(list(pca = po_pca, nothing = po_nop))
#' # gives the same as
#' branches = c("pca", "nothing")
#' po("branch", branches) %>>%
#'   gunion(list(po_pca, po_nop)) %>>%
#'   po("unbranch", branches)
#'
#' pipeline_branch(list(pca = po_pca, nothing = po_nop),
#'   prefix_branchops = "br_", prefix_paths = "xy_")
#' # gives the same as
#' po("branch", branches, id = "br_branch") %>>%
#'   gunion(list(xy_pca = po_pca, xy_nothing = po_nop)) %>>%
#'   po("unbranch", branches, id = "br_unbranch")
pipeline_branch = function(graphs, prefix_branchops = "", prefix_paths = FALSE) {
  assert_list(graphs, null.ok = TRUE)
  assert_string(prefix_branchops)
  assert(
    check_flag(prefix_paths),
    check_string(prefix_paths)
  )
  assert(
    check_list(graphs, min.len = 1, any.missing = FALSE, names = "unique"),
    check_list(graphs, min.len = 1, any.missing = FALSE, names = "unnamed")
  )

  graphs = lapply(graphs, as_graph)
  imap(graphs, function(g, idx) {
    if (nrow(g$output) != 1) {
      stopf("Graph %s must have exactly one output channel", idx)
    }
  })

  graphs_input = graphs

  branches = if (is.null(names(graphs))) length(graphs) else names(graphs)
  if (!isFALSE(prefix_paths)) {
    if (is.null(names(graphs))) {
      names(graphs) = paste0("po", as.character(seq_along(graphs)))
    }
    if (is.character(prefix_paths)) {
      names(graphs) = paste0(prefix_paths, names(graphs))
    }
    poname_prefix = paste0(names(graphs), ".")
  } else {
    names(graphs) = NULL
    poname_prefix = ""
  }

  graph = gunion(graphs) %>>!% PipeOpUnbranch$new(branches, id = paste0(prefix_branchops, "unbranch"))

  branch_id = paste0(prefix_branchops, "branch")
  po_branch = PipeOpBranch$new(branches, id = branch_id)
  graph$add_pipeop(po_branch)

  pmap(list(graphs, poname_prefix, po_branch$output$name), function(gr, pnp, branch_chan) {
    gin = gr$input
    gin$op.id = paste0(pnp, gin$op.id)

    pmap(list(
      src_id = branch_id, dst_id = gin$op.id,
      src_channel = branch_chan, dst_channel = gin$channel.name),
      graph$add_edge)
  })
  graph
}

mlr_graphs$add("branch", pipeline_branch)

