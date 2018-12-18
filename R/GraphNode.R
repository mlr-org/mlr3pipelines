
GraphElement = R6::R6Class("GraphElement",
  active = list(
      intype = function() stop("abstract"),  # list of character
      outtype = function() stop("abstract"),  # list of character
      in_edges = function() stop("abstract"),  # list of GraphEdge
      out_edges = function() stop("abstract")  # list of GraphEdge
  )
)

GraphEdge = R6::R6Class("GraphEdge",
  public = list(
      name = NULL,
      element = NULL,
      direction = NULL,
      initialize = function(name, element, direction) {
        assert_choice(direction, c("in", "out"))
        self$name = name
        self$element = element
        self$direction = direction
      },
      print = function() {
        catf("%s-Edge name %s into GraphNode %s", self$direction, self$name, self$element$pipeop$id)
      }
  )
)

numbername = function(li) {
  res = lapply(seq_along(li), function(x) {
    ret = names2(li)[x]
    if (is.na(ret)) x else ret
  })
  names(res) = names(li)
  res
}

#### Class definition ####

#' GraphNode
#'
#' @description
#'
#' The \code{GraphNode} class implements behaviors required to define the
#' connections between the \code{PipeOps}. The set of interconnected
#' GraphNodes creates the pipeline graph.
#'
#' @usage GraphNode$new(pipeop)
#'
#' @importFrom R6 R6Class
#'
#'
GraphNode = R6::R6Class("GraphNode",
  inherit = GraphElement,

  public = list(

    initialize = function(pipeop, graph) {
      private$.pipeop = pipeop
      private$.graph = graph
      private$.next_node_edges = sapply(pipeop$intype, function(.) NULL, simplify = FALSE)
      private$.prev_node_edges = sapply(pipeop$outtype, function(.) NULL, simplify = FALSE)
      private$.in_edges = sapply(numbername(pipeop$intype), GraphEdge$new, element = self, simplify = FALSE, direction = "in")
      private$.out_edges = sapply(numbername(pipeop$outtype), GraphEdge$new, element = self, simplify = FALSE, direction = "out")
      graph$add_node(self)
      self
    },

    print = function(...) {
      BBmisc::catf("GraphNode: <%s>", self$pipeop$id)
    }

  ),
  private = list(
      .next_node_edges = NULL,
      .prev_node_edges = NULL,
      .editlock = FALSE,
      .in_edges = NULL,
      .out_edges = NULL,
      .graph = NULL,
      .pipeop = NULL
  ),
  active = list(
      graph = function() private$.graph,
      pipeop = function() private$.pipeop,
      prev_node_edges = function(prev) {
        if (!missing(prev)) {
          connectgn(prev, ".prev_node_edges", "next_node_edges", "in", self, private)
        }
        private$.prev_node_edges
      },
      next_node_edges = function(nxt) {
        if (!missing(nxt)) {
          connectgn(nxt, ".next_node_edges", "prev_node_edges", "out", self, private)
        }
        private$.next_node_edges
      },
      next_nodes = function() map(self$next_node_edges, "element"),
      prev_nodes = function() map(self$prev_node_edges, "element"),
      in_edges = function() private$.in_edges,
      out_edges = function() private$.out_edges,
      intype = function() self$pipeop$intype,
      outtype = function() self$pipeop$outtype,
    input_complete = function() !any(sapply(self$prev_nodes, is.null)),
    output_complete = function() !any(sapply(self$next_nodes, is.null))
  )
)

connectgn = function(newedges, oldedgename, inverseedgename, direction, self, private) {
  catf("connectgn into %s, other side %s, direction %s. lock %s", oldedgename, inverseedgename, direction, private$.editlock)
  # TODO: assert prev is a list
  if (!identical(names(newedges), names(private[[oldedgename]]))) {
    stop("Can't change names of nodes")
  }
  if (private$.editlock) return(NULL)
  on.exit({private$.editlock = FALSE})
  private$.editlock = TRUE
  for (edge in newedges) {
    # todo: assert edge is a GraphEdge
    # TODO: check types
    if (!identical(edge$element$graph, private$.graph)) {
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
    oldedge$element$next_nodes[[oldedge$name]] = NULL
    newedges[[idx]]$element[[inverseedgename]][[newedges[[idx]]$name]] = GraphEdge$new(edgename, self, direction)
  }
  private[[oldedgename]] = newedges
  self$graph$update_connections()
}
