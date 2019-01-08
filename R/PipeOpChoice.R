
#' @title PipeOpChoice
#' @format [R6Class] PipeOpChoice
#'
#' @description
#' This is one choice PipeOp! Used for multiplexing between different
#' possible paths.
#'
#' @section Methods
#' * `new(options = 1)` \cr
#'   (`integer(1)` | `character`) -> [`PipeOpChoice`]
#'
#' @section Parameters:
#' * `selection`: integer or discrete
#'
#' @section Details:
#' Creates a PipeOp with multiple output channels that can be used to
#' create a Graph network with alternative paths. If `options` is an `integer(1)`,
#' it determines the number of out-paths and `selection` is an integer parameter
#' choosing between these paths. If `options` is a `character`, then `length(options)`
#' out channels are created, each named according to `options`.
#'
#' To create a usable graph, the branching paths need to be brought together
#' using [`PipeOpUnchoice`].
#'
#' @examples
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpChoice$new(choices) %>>% gunion(pca, nop) %>>% PipeOpUnchoice$new(choices)
#'
#' @family PipeOp
#' @export

#' @title PipeOpUnchoice
#' @format [R6Class] PipeOpUnchoice
#'
#' @description
#' Used to bring together different paths created by [`PipeOpChoice`].
#'
#' @section Methods
#' * `new(options = 1)` \cr
#'   (`integer(1)` | `character`) -> [`PipeOpChoice`]
#'
#' @section Details:
#' Creates a PipeOp with multiple input channels that can be used to
#' create a Graph network with alternative paths. `options` works as in [`PipeOpChoice`]
#' and should probably be the same value as the `options` given to the corresponding
#' [`PipeOpChoice`] instance.
#'
#' @examples
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpChoice$new(choices) %>>% gunion(pca, nop) %>>% PipeOpUnchoice$new(choices)
#'
#' @family PipeOp
#' @export

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
#'   be given.
#' @examples
#' grultiplex(pca = PipeOpPCA$new(), nop = PipeOpNULL$new())
#' # gives the same as
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpChoice$new(choices) %>>% gunion(pca, nop) %>>% PipeOpUnchoice$new(choices)
#' @export
grultiplex <- function(..., .graph = NULL) {
  assert_list(.graphs, nullok = TRUE)
  graphs <- c(list(...), .graph)
  assert(
      check_list(graphs, min.len = 1, types = c("PipeOp", "Graph"), any.missing = FALSE, names = "unique"),
      check_list(graphs, min.len = 1, types = c("PipeOp", "Graph"), any.missing = FALSE, names = "unnamed")
  )

  graphs = lapply(graphs, Graph$new)
  imap(graphs, function(g, idx) {
    if (length(g$in_channels != 1)) {
      stopf("Graph %s must have exactly one in_channel", idx)
    }
    if (length(g$out_channels != 1)) {
      stopf("Graph %s must have exactly one out_channel", idx)
    }
  })

  choices = if (is.null(names(graphs))) length(graphs) else names(graphs)
  PipeOpChoice() %>>% gunion(.graphs = graphs) %>>% PipeOpUnchoice()
}
