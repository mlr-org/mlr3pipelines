#' @include utils.R

NodeChannel = R6::R6Class("NodeChannel",
  public = list(
    name = NULL,
    node = NULL,
    direction = NULL,
    initialize = function(name, node, direction) {
      assert_choice(direction, c("in", "out"))
      self$name = name
      self$node = node
      self$direction = direction
    },
    print = function() {
      catf("Channel name %s %s GraphNode %s", self$name, if (self$direction == "in") "into" else "out of", self$node$pipeop$id)
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
      .editlock = FALSE,
      .in_channels = NULL,
      .out_channels = NULL,
      .graph = NULL,
      .pipeop = NULL
  ),
  active = list(
      graph = readonly("graph"),
      pipeop = readonly("pipeop"),
      prev_node_channels = function(prev) {
        if (!missing(prev)) {
          private$connectgn(prev, ".prev_node_channels", "next_node_channels", "in")
        }
        private$.prev_node_channels
      },
      next_node_channels = function(nxt) {
        if (!missing(nxt)) {
          private$connectgn(nxt, ".next_node_channels", "prev_node_channels", "out")
        }
        private$.next_node_channels
      },
      next_nodes = function() map(self$next_node_channels, "node"),
      prev_nodes = function() map(self$prev_node_channels, "node"),
      in_channels = function() private$.in_channels,
      out_channels = function() private$.out_channels,
      intype = function() self$pipeop$intype,
      outtype = function() self$pipeop$outtype,
    input_complete = function() !any(sapply(self$prev_nodes, is.null)),
    output_complete = function() !any(sapply(self$next_nodes, is.null))
  )
)

GraphNode$set("private", "connectgn", function(newedges, oldedgename, inverseedgename, direction) {
  # TODO: assert prev is a list
  if (!identical(names(newedges), names(private[[oldedgename]]))) {
    stop("Can't change names of nodes")
  }
  if (private$.editlock) return(NULL)
  on.exit({private$.editlock = FALSE})
  private$.editlock = TRUE
  for (edge in newedges) {
    # todo: assert edge is a NodeChannel
    # TODO: check types
    if (!is.null(edge) && !identical(edge$node$graph, private$.graph)) {
      stop("Can't connect nodes that are not in the same graph")
    }
  }
  for (idx in seq_along(newedges)) {
    oldedge = private[[oldedgename]][[idx]]
    if (identical(newedges[[idx]], oldedge)) {
      next
    }

    edgename = names2(newedges)[[idx]]
    if (is.na(edgename)) edgename = idx

    if (!is.null(oldedge)) oldedge$node[[inverseedgename]][oldedge$name] = list(NULL)
    if (!is.null(newedges[[idx]])) {
      newedges[[idx]]$node[[inverseedgename]][[newedges[[idx]]$name]] = NodeChannel$new(edgename, self, direction)
    }
  }
  private[[oldedgename]] = newedges
  self$graph$update_connections()
})

numbername = function(li) {
  res = lapply(seq_along(li), function(x) {
    ret = names2(li)[x]
    if (is.na(ret)) x else ret
  })
  names(res) = names(li)
  res
}
