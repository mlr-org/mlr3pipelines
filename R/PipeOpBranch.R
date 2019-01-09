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
      super$initialize(id, ParamSet$new(params = list(param)))
      self$train_intypes = "any"
      self$train_outtypes = rep("any", outnum)
      self$predict_intypes = "any"
      self$predict_outtypes = rep("any", outnum)
      private$.defaultreturn = rep(list(NULL), outnum)
      private$.outnum = outnum
      if (is.character(options)) {
        names(private$.outtype) = options
        names(private$.defaultreturn) = options
      }
    },
    train = function(input) {
      assert_list(input, len = private$.outnum)
      self$state = list()
      ret = private$.defaultreturn
      ret[[self$param_vals[[1]]]] = input[[1]]
      return(ret)
    },
    predict = function(input) {
      assert_list(input, len = private$.outnum)
      ret = private$.defaultreturn
      ret[[self$param_vals[[1]]]] = input[[1]]
      return(ret)
    }
  ),
  private = list(
    .defaultreturn = NULL,  # list of NULLs with the correct length and names
    .outnum = NULL
  ),
  active = list(
    outnum = function() private$.outnum
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
      super$initialize(id)
      private$.innum = innum
      self$train_intypes = rep("any", innum)
      self$train_outtypes = "any"
      self$predict_intypes = rep("any", innum)
      self$predict_outtypes = "any"
    },
    train = function(input) {
      assert_list(input, len = private$.innum)
      self$state = list()
      nonnull = Filter(Negate(is.null), input)
      assert_list(nonnull, any.missing = FALSE, len = 1)
      return(nonnull)
    },
    predict = function(input) {
      assert_list(input, len = private$.innum)
      nonnull = Filter(Negate(is.null), input)
      assert_list(nonnull, any.missing = FALSE, len = 1)
      return(nonnull)
    }
  ),
  private = list(
   .innum = NULL
  ),
  active = list(
    innum = function() private$.innum
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
