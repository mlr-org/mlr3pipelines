#' @include utils.R

# Identifies the exact point to which data is delivered when coming out of an operation
# (or, inversely, the point /from which/ data is coming when flowing into a node).
# This identifies the `node`, and the index or channel_id of this node to which
# (out of which) data is flowing.
#
# `node` [GraphNode] Node that is connected to
# `channel_id` [character(1) | numeric(1)] index or name of connected node's channel
# `direction` [character(1)] "in" or "out"
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

#### Class definition ####

#' GraphNode
#'
#' @description
#'
#' The \code{GraphNode} class implements behaviors required to define the
#' connections between the \code{PipeOps}. The set of interconnected
#' GraphNodes creates the pipeline graph.
#'
#' @usage GraphNode
#'
#' @importFrom R6 R6Class
#'
#'
GraphNode = R6::R6Class("GraphNode",
  public = list(
    initialize = function(pipeop, graph) {
      private$.pipeop = pipeop
      private$.graph = graph
      private$.next_node_channels = sapply(pipeop$outtype, function(.) NULL, simplify = FALSE)
      private$.prev_node_channels = sapply(pipeop$intype, function(.) NULL, simplify = FALSE)
      private$.in_channels = sapply(numbername(pipeop$intype), NodeChannel$new, node = self, simplify = FALSE, direction = "in")
      private$.out_channels = sapply(numbername(pipeop$outtype), NodeChannel$new, node = self, simplify = FALSE, direction = "out")
      graph$add_node(self)
      self
    },

    print = function(...) {
      catf("GraphNode: <%s>", self$pipeop$id)
    }
  ),
  private = list(
      .next_node_channels = NULL,
      .prev_node_channels = NULL,
      .editlock = FALSE,  # prevent endless loops when updating node connections, see `connectgn`
      .in_channels = NULL,
      .out_channels = NULL,
      .graph = NULL,
      .pipeop = NULL
  ),
  active = list(
    graph = readonly("graph"),  # [Graph] that this GraphNode is a member of
    pipeop = readonly("pipeop"),  # [PipeOp] that this GraphNode contains
    prev_node_channels = function(prev) {
      # [list of NodeChannel]
      # Identifies the channels from previous nodes that we are connected to.
      # Mutation can be used to change connections.
      if (!missing(prev)) {
        private$connectgn(prev, ".prev_node_channels", "next_node_channels", "in")
      }
      private$.prev_node_channels
    },
    next_node_channels = function(nxt) {
      # [list of NodeChannel]
      # Identifies the channels from successor nodes that we are connected to.
      # Mutation can be used to change connections.
      if (!missing(nxt)) {
        private$connectgn(nxt, ".next_node_channels", "prev_node_channels", "out")
      }
      private$.next_node_channels
    },
    next_nodes = function() map(self$next_node_channels, "node"),  # [list of GraphNode, indexed by channel_id] Successor nodes
    prev_nodes = function() map(self$prev_node_channels, "node"),  # [list of GraphNode, indexed by channel_id] Predecessor nodes
    in_channels = function() private$.in_channels,  # [list of GraphChannel, indexed by channel_id] cached identifiers of incoming nodes
    out_channels = function() private$.out_channels,  # [list of GraphChannel, indexed by channel_id] cached identifiers of outgoing nodes
    intype = function() self$pipeop$intype,  # [list of ..., indexed by channel_id] Identifies types of incoming data, see PipeOp->intype
    outtype = function() self$pipeop$outtype,  # [list of ..., indexed by channel_id] Identifies types of outgoing data, see PipeOp->outtype
    input_complete = function() !any(sapply(self$prev_nodes, is.null)),  # [logical(1)] Whether all incoming channels are connected to other nodes
    output_complete = function() !any(sapply(self$next_nodes, is.null))  # [logical(1)] Whether all outgoing channels are connected to other nodes
  )
)

# connectgn: change prev_node_channels or next_node_channels (and update other node's connections
# to point back to us)
# @param newedges: [list of NodeChannel, indexed by channel_id] new channels to connect to
# @param oldedgename [character(1)]: slot name of `private` that gives the related old list of GraphNode, indexed by channel_id.
#   (probably ".next_node_channels" or ".prev_node_channels")
# @param inverseedgename [character(1)]: slot name other nodes that we could be connected to. (We need to access OTHERNODE[[inverseedgename]]).
#   (probably "prev_node_channels" or "next_node_channels" -- always the one OPPOSITE of `oldedgename`
# @param direction [character(1)] "in" or "out". "in" if we are changing prev_node_channels, otherwise "out".
GraphNode$set("private", "connectgn", function(newedges, oldedgename, inverseedgename, direction) {
  # TODO: assert prev is a list
  if (!identical(names(newedges), names(private[[oldedgename]]))) {
    stop("Can't change names of nodes")
  }

  # editlock: Prevent re-entry into node change.
  #   when someone changes a connection on this node, we are going to change the reverse connection on
  #   other nodes that are connected to us. (Make a newly connected node point to us, make a node that is not
  #   connected *any more* point to NULL). We need to make sure there is no infinite loop when the other node
  #   then tries to call our node *again*, so we set an "edit lock".
  if (private$.editlock) return(NULL)
  on.exit({private$.editlock = FALSE})
  private$.editlock = TRUE

  # Check that input is okay before we start changing edges
  for (edge in newedges) {
    # todo: assert edge is a NodeChannel
    # TODO: check types
    if (!is.null(edge) && !identical(edge$node$graph, private$.graph)) {
      stop("Can't connect nodes that are not in the same graph")
    }
  }

  # Updating opposite node's connections here
  for (idx in seq_along(newedges)) {
    oldchannel = private[[oldchannelname]][[idx]]
    if (identical(newedges[[idx]], oldchannel)) {
      # connection did not change
      next
    }
    if (!is.null(oldchannel)) {  # only if we were previously connected to smth
      # Make the node that was previously connected to us point to NULL.
      oldchannel$node[[inverseedgename]][oldchannel$channel_id] = list(NULL)
    }
    if (!is.null(newedges[[idx]])) {
      # Make the node that we are connecting with point back to us
      current_channel_id = names2(newedges)[[idx]]
      if (is.na(current_channel_id)) current_channel_id = idx
      newedges[[idx]]$node[[inverseedgename]][[newedges[[idx]]$channel_id]] = NodeChannel$new(current_channel_id, self, direction)
    }
  }
  # actually change the [prev/next]_node_channels
  private[[oldchannelname]] = newedges

  self$graph$update_connections()  # update the graph's info about unconnected nodes
})

numbername = function(li) {
  res = lapply(seq_along(li), function(x) {
    ret = names2(li)[x]
    if (is.na(ret)) x else ret
  })
  names(res) = names(li)
  res
}
