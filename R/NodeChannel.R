#' @title NodeChannel
#'
#' @description
#' Identifies the exact point to which data is delivered when coming out of an operation
#' (or, inversely, the point /from which/ data is coming when flowing into a node).
#' This identifies the `node`, and the index or channel_id of this node to which
#' (out of which) data is flowing.
#'
#' @section Public Members / Active Bindings
#' * `node`                       :: [GraphNode]
#'   Node that the channel is connected to.
#' * `channel_id`                 :: [character(1)] | [numeric(1)]
#'   Index or name of connected node's channel.
#' * `direction`                  :: [character(1)]
#'   Can be "in" or "out".
#'
#' @name PipeOp
#' @family PipeOp
NodeChannel = R6::R6Class("NodeChannel",
  public = list(
    channel_id = NULL,
    node = NULL,
    direction = NULL,

    initialize = function(channel_id, node, direction) {
      assert_choice(direction, c("in", "out"))
      self$channel_id = channel_id
      self$node = node
      self$direction = direction
    },

    print = function() {
      catf("Channel with id [%s] %s GraphNode %s", self$channel_id, if (self$direction == "in") "into" else "out of", self$node$pipeop$id)
    }
  )
)

