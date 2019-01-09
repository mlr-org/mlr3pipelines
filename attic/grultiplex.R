#' @title grultiplex
#'
#' @description
#' Create a multiplexed graph.
#'
#' @param `...`:
#'   Multiple graphs, possibly named. They all must have exactly one in_channel and
#'   exactly one out_channel. If any of the arguments are named, then all must have
#'   unique names.
#' @param `.graphs` ([list of Graph]):
#'   Additionally to the graphs given in `...`, a (posibly named) list of graphs can
#'   be given. Default is `NULL`.
#' @param `.id` ([character(1)]):
#'   Optional id prefix to prepend to [`PipeOpBranch`] and [`PipeOpUnbranch`] id. Their
#'   resulting IDs will be `"choice"` and `"unchoice"`, prefixed by `.id`. Default is `""`.
#' @param `.prefix.gunion.names` ([logical(1)]):
#'   Whether to add prefixes to graph IDs when performing gunion. Can be helpful to
#'   avoid ID clashes in resulting graph. Default `FALSE`.
#' @examples
#' grultiplex(pca = PipeOpPCA$new(), nop = PipeOpNULL$new())
#' # gives the same as
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpBranch$new(choices) %>>% gunion(pca, nop) %>>% PipeOpUnbranch$new(choices)
#' @export
grultiplex <- function(..., .graphs = NULL, .id = "", .prefix.gunion.names = FALSE) {
  assert_list(.graphs, null.ok = TRUE)
  graphs <- c(list(...), .graphs) ; rm(.graphs)
  assert(
      check_list(graphs, min.len = 1, types = c("PipeOp", "Graph"), any.missing = FALSE, names = "unique"),
      check_list(graphs, min.len = 1, types = c("PipeOp", "Graph"), any.missing = FALSE, names = "unnamed")
  )

  graphs = lapply(graphs, Graph$new)
  imap(graphs, function(g, idx) {
    if (length(g$in_channels) != 1) {
      stopf("Graph %s must have exactly one in_channel", idx)
    }
    if (length(g$out_channels) != 1) {
      stopf("Graph %s must have exactly one out_channel", idx)
    }
  })

  choices = if (is.null(names(graphs))) length(graphs) else names(graphs)
  if (.prefix.gunion.names) {
    if (is.null(names(graphs))) {
      names(graphs) = as.character(seq_along(graphs))
    }
  } else {
    names(graphs) = NULL
  }
  PipeOpBranch$new(choices, id = paste0(.id, "choice")) %>>%
    gunion(list(graphs)) %>>%
    PipeOpUnbranch$new(choices, id = paste0(.id, "unchoice"))
}
