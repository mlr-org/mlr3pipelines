#' @title PipeOpBranch
#'
#' @name mlr_pipeop_branch
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' This pipeop is used for multiplexing between different possible paths and
#' should be used in conjunction with [`PipeOpUnbranch`].
#'
#' @section Methods:
#' * `PipeOpBranch$new(options, id = "branch")` \cr
#'   (`numeric(1)` | `character`, `character(1)`) -> `self` \cr
#'   Constructor. If `options` is an integer number, it determines the number of
#'   output channels / options that are created, named `output1`...`output<n>`. The
#'   `$selection` parameter will then be a [`ParamInt`].
#'   If `options` is a `character`, it determines the names of channels directly.
#'   The `$selection` parameter will then be a [`ParamFct`].
#'
#' @section Parameter Set:
#' * `selection`: (`numeric(1)` | `character(1)`) \cr
#'   Selection of branching path to take. Is a `ParamInt` if the `options` parameter
#'   during construction was a `numeric(1)`, and ranges from 1 to `options`. Is a
#'   `ParamFct` if the `options` parameter was a `character` and its possible values
#'   are the `options` values.
#'
#' @section Details:
#' Creates a PipeOp with multiple output channels that can be used to
#' create a Graph network with alternative paths. If `options` is an `integer(1)`,
#' it determines the number of out-paths and `selection` is an integer parameter
#' choosing between these paths. If `options` is a `character`, then `length(options)`
#' out channels are created, each named according to `options`.
#'
#' To create a usable graph, the branching paths need to be brought together
#' using [`PipeOpUnbranch`].
#'
#' Not to be confused with [`PipeOpCopy`], the naming scheme is a bit unfortunate.
#'
#' @family PipeOps
#' @family Path Branching
#' @include PipeOp.R
#' @export
#' @examples
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpBranch$new(choices) %>>% gunion(list(pca, nop)) %>>% PipeOpUnbranch$new(choices)
PipeOpBranch = R6Class("PipeOpBranch",
  inherit = PipeOp,
  public = list(
    initialize = function(options, id = "branch", param_vals = list()) {
      assert(
        check_int(options, lower = 1L),
        check_character(options, min.len = 1L, any.missing = FALSE)
      )
      if (is.numeric(options)) {
        options = round(options)
        param = ParamInt$new("selection", lower = 1L, upper = options, default = 1L)
        options = rep_suffix("output", options)
      } else {
        param = ParamFct$new("selection", levels = options, default = options[1L])
      }
      ps = ParamSet$new(params = list(param))
      ps$values$selection = ps$params$selection$default
      super$initialize(id, ps, param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = options, train = "*", predict = "*")
      )
    },

    train_internal = function(inputs) {
      self$state = list()
      ret = named_list(self$output$name, NO_OP)
      ret[[self$param_set$values$selection]] = inputs[[1]]
      ret
    },

    predict_internal = function(inputs) {
      assert_list(inputs)
      ret = named_list(self$output$name, NO_OP)
      ret[[self$param_set$values$selection]] = inputs[[1]]
      ret
    }
  )
)

mlr_pipeops$add("branch", PipeOpBranch, list("N"))

#' @title Branch Between Alternative Paths
#'
#' @description
#' Create a multiplexed graph.
#'
#' @param ...
#'   Multiple graphs, possibly named. They all must have exactly
#'   one output. If any of the arguments are named, then all must have
#'   unique names.
#' @param .graphs (`[list of Graph]`):
#'   Named list of Graphs, additionally to the graphs given in `...`.
#' @param .prefix_branchops (`[character(1)]`):
#'   Optional id prefix to prepend to [`PipeOpBranch`] and [`PipeOpUnbranch`] id. Their
#'   resulting IDs will be `"[.prefix_branchops]branch"` and `"[.prefix_branchops]unbranch"`.
#'   Default is `""`.
#' @param .prefix_paths (`[logical(1) | character(1)]`):
#'   Whether to add prefixes to graph IDs when performing gunion. Can be helpful to
#'   avoid ID clashes in resulting graph. Default `FALSE`. If this is `TRUE`, the prefixes
#'   are taken from the names of the input arguments if present or `"poX"` where X counts up. If this is
#'   a `character(1)`, it is a prefix that is added to the `PipeOp` IDs *additionally*
#'   to the input argument list.
#' @examples
#' po_pca = PipeOpPCA$new()
#' po_nop = PipeOpNULL$new()
#'
#' branch(pca = po_pca, nothing = po_nop)
#' # gives the same as
#' branches = c("pca", "nothing")
#' PipeOpBranch$new(branches) %>>% gunion(list(po_pca, po_nop)) %>>% PipeOpUnbranch$new(branches)
#'
#' branch(pca = po_pca, nothing = po_nop, .prefix_branchops = "br_", .prefix_paths = "xy_")
#' #gives the same as
#' PipeOpBranch$new(branches, id = "br_branch") %>>%
#'   gunion(list(xy_pca = po_pca, xy_nothing = po_nop)) %>>%
#'   PipeOpUnbranch$new(branches, id = "br_unbranch")
#'
#' @export
branch <- function(..., .graphs = NULL, .prefix_branchops = "", .prefix_paths = FALSE) {
  assert_list(.graphs, null.ok = TRUE)
  assert_string(.prefix_branchops)
  assert(
    check_flag(.prefix_paths),
    check_string(.prefix_paths)
  )
  graphs <- c(list(...), .graphs) ; rm(.graphs)
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
  if (!isFALSE(.prefix_paths)) {
    if (is.null(names(graphs))) {
      names(graphs) = paste0("po", as.character(seq_along(graphs)))
    }
    if (is.character(.prefix_paths)) {
      names(graphs) = paste0(.prefix_paths, names(graphs))
    }
    poname_prefix = paste0(names(graphs), ".")
  } else {
    names(graphs) = NULL
    poname_prefix = ""
  }

  graph = gunion(list(graphs)) %>>% PipeOpUnbranch$new(branches, id = paste0(.prefix_branchops, "unbranch"))

  branch_id = paste0(.prefix_branchops, "branch")
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
