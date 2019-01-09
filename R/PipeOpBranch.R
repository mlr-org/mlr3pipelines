#' @title PipeOpBranch
#' @format [R6Class] PipeOpBranch
#'
#' @description
#' This pipeop is used for multiplexing between different possible paths.
#'
#' @section Methods:
#' * `new(options = 1, id = "choice")` \cr
#'   (`integer(1)` | `character`), `character(1)` -> [`PipeOpBranch`]
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
#' using [`PipeOpUnbranch`].
#'
#' @examples
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpBranch
#' $new(choices) %>>% gunion(pca, nop) %>>% PipeOpUnbranch$new(choices)
#'
#' @family PipeOp, PipeOpBroadcast
#' @export
PipeOpBranch = R6::R6Class("PipeOpBranch",
  inherit = PipeOp,
  public = list(
    outnum = NULL,
    initialize = function(options, id = "branch") {
      assert(
        check_int(options, lower = 1),
        check_character(options, min.len = 1, any.missing = FALSE)
      )
      if (is.numeric(options)) {
        options = round(options)
        param = ParamInt$new("selection", lower = 1, upper = options, default = 1)
        outnum = options
      } else {
        param = ParamFct$new("selection", values = options, default = options[1])
        outnum = length(options)
      }
      super$initialize(id,
        param_set = ParamSet$new(params = list(param)),
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = rep_suffix("input", outnum), train = "*", predict = "*")
      )
      self$outnum = outnum
    },

    train = function(inputs) {
      assert_list(inputs)
      self$state = list()
      ret = named_list(self$output$name)
      ret[[self$param_vals[[1L]]]] = inputs[[1L]]
      return(ret)
    },

    predict = function(inputs) {
      assert_list(inputs)
      ret = named_list(self$output$name)
      ret[[self$param_vals[[1L]]]] = inputs[[1L]]
      return(ret)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpBranch", PipeOpBranch)


#' @title PipeOpUnbranch
#' @format [R6Class] PipeOpUnbranch
#'
#' @description
#' Used to bring together different paths created by [`PipeOpBranch`].
#'
#' @section Methods:
#' * `new(options = 1)` \cr
#'   (`integer(1)` | `character`) -> [`PipeOpBranch`]
#'
#' @section Details:
#' Creates a PipeOp with multiple input channels that can be used to
#' create a Graph network with alternative paths. `options` works as in [`PipeOpBranch`]
#' and should probably be the same value as the `options` given to the corresponding
#' [`PipeOpBranch`] instance.
#'
#' @examples
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpUnbranch$new(choices) %>>% gunion(pca, nop) %>>% PipeOpUnbranch$new(choices)
#'
#' @family PipeOp, PipeOpAggregate
#' @export
PipeOpUnbranch = R6::R6Class("PipeOpUnbranch",
  inherit = PipeOp,
  public = list(
    innum = NULL,
    initialize = function(options, id = "unbranch") {
      assert(
        check_int(options, lower = 1),
        check_character(options, min.len = 1, any.missing = FALSE)
      )
      if (is.numeric(options)) {
        options = round(options)
        innum = options
      } else {
        innum = length(options)
      }
      super$initialize(id,
        input = data.table(name = rep_suffix("input", innum), train = "*", predict = "*"),
        output = data.table(name = "output", train = "*", predict = "*")
      )
      self$innum = innum
    },

    train = function(inputs) {
      assert_list(inputs, len = self$innum)
      self$state = list()
      nonnull = Filter(Negate(is.null), inputs)
      assert_list(nonnull, any.missing = FALSE, len = 1)
      return(nonnull)
    },

    predict = function(inputs) {
      assert_list(inputs, len = self$innum)
      nonnull = Filter(Negate(is.null), inputs)
      assert_list(nonnull, any.missing = FALSE, len = 1)
      return(nonnull)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpUnbranch", PipeOpUnbranch)

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
