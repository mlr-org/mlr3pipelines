#' @title PipeOpBranch
#'
#' @usage NULL
#' @name mlr_pipeops_branch
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Perform alternative path branching: [`PipeOpBranch`] has multiple output channels
#' that connect to different paths in a [`Graph`]. At any time, only one of these
#' paths will be taken for execution. At the end of the different paths, the
#' [`PipeOpUnbranch`] `PipeOp` must be used to indicate the end of alternative paths.
#'
#' Not to be confused with [`PipeOpCopy`], the naming scheme is a bit unfortunate.
#'
#' @section Construction:
#' ```
#' PipeOpBranch$new(options, id = "branch", param_vals = list())
#' ```
#' * `options` :: `numeric(1)` | `character`\cr
#'   If `options` is an integer number, it determines the number of
#'   output channels / options that are created, named `output1`...`output<n>`. The
#'   `$selection` parameter will then be a [`ParamInt`].
#'   If `options` is a `character`, it determines the names of channels directly.
#'   The `$selection` parameter will then be a [`ParamFct`].
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"branch"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output:
#' [`PipeOpBranch`] has one input channel named `"input"`, taking any input (`"*"`) both during training and prediction.
#'
#' [`PipeOpBranch`] has multiple output channels depending on the `options` construction argument, named `"output1"`, `"output2"`, ...
#' if `options` is `numeric`, and named after each `options` value if `options` is a `character`.
#' All output channels produce the object given as input (`"*"`) or [`NO_OP`], both during training and prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `selection` :: `numeric(1)` | `character(1)`\cr
#'   Selection of branching path to take. Is a `ParamInt` if the `options` parameter
#'   during construction was a `numeric(1)`, and ranges from 1 to `options`. Is a
#'   `ParamFct` if the `options` parameter was a `character` and its possible values
#'   are the `options` values. Initialized to either 1 (if the `options` construction argument is `numeric(1)`)
#'   or the first element of `options` (if it is `character`).
#'
#' @section Internals:
#' Alternative path branching is handled by the [`PipeOp`] backend. To indicate that
#' a path should not be taken, [`PipeOpBranch`] returns the [`NO_OP`] object on its
#' output channel. The [`PipeOp`] handles each [`NO_OP`] input by automatically
#' returning a [`NO_OP`] output without calling `$.train()` or `$.predict()`,
#' until [`PipeOpUnbranch`] is reached. [`PipeOpUnbranch`] will then take multiple inputs,
#' all except one of which must be a [`NO_OP`], and forward the only non-[`NO_OP`]
#' object on its output.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Path Branching
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#'
#' pca = po("pca")
#' nop = po("nop")
#' choices = c("pca", "nothing")
#' gr = po("branch", choices) %>>%
#'   gunion(list(pca, nop)) %>>%
#'   po("unbranch", choices)
#'
#' gr$param_set$values$branch.selection = "pca"
#' gr$train(tsk("iris"))
#'
#' gr$param_set$values$branch.selection = "nothing"
#' gr$train(tsk("iris"))
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
        param = ParamInt$new("selection", lower = 1L, upper = options, tags = c("train", "predict", "required"))
        options = rep_suffix("output", options)
        initval = 1
      } else {
        param = ParamFct$new("selection", levels = options, tags = c("train", "predict", "required"))
        initval = options[1]
      }
      ps = ParamSet$new(params = list(param))
      ps$values$selection = initval
      super$initialize(id, ps, param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = options, train = "*", predict = "*"),
        tags = "meta"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      ret = named_list(self$output$name, NO_OP)
      ret[[self$param_set$values$selection]] = inputs[[1]]
      ret
    },
    .predict = function(inputs) {
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
#'
#' @export
#' @examples
#' library("mlr3")
#'
#' po_pca = po("pca")
#' po_nop = po("nop")
#'
#' branches = branch(pca = po_pca, nothing = po_nop)
#' # gives the same as
#' branches = c("pca", "nothing")
#' po("branch", branches) %>>%
#'   gunion(list(po_pca, po_nop)) %>>%
#'   po("unbranch", branches)
#'
#' branch(pca = po_pca, nothing = po_nop,
#'   .prefix_branchops = "br_", .prefix_paths = "xy_")
#' # gives the same as
#' po("branch", branches, id = "br_branch") %>>%
#'   gunion(list(xy_pca = po_pca, xy_nothing = po_nop)) %>>%
#'   po("unbranch", branches, id = "br_unbranch")
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
