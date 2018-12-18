
GraphElement = R6::R6Class("GraphElement",
  active = list(
      intype = function() stop("abstract"),  # list of character
      outtype = function() stop("abstract"),  # list of character
      inputs = function() stop("abstract"),  # list of GraphEdge
      outputs = function() stop("abstract")  # list of GraphEdge
  )
)

GraphEdge = R6::R6Class("GraphEdge",
  public = list(
      name = NULL,
      element = NULL,
      initialize = function(name, element) {
        self$name = name
        self$element = element
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
#' @usage GraphNode$new(pipeop)
#'
#' @importFrom R6 R6Class
#'
#'
GraphNode = R6::R6Class("GraphNode",
  inherits = GraphElement,

  public = list(
    pipeop = NULL,
    next_nodes = NULL,
    prev_nodes = NULL,
    initialize = function(pipeop, graph) {
      self$pipeop = pipeop
      self$graph = graph
      self$.next_nodes = sapply(pipeop$intype, function(.) NULL, simplify = FALSE)
      self$.prev_nodes = sapply(pipeop$outtype, function(.) NULL, simplify = FALSE)
      self$.in_edges = sapply(names(pipeop$intype), GraphEdge$new, element = self, simplify = FALSE)
      self$.out_edges = sapply(names(pipeop$outtype), GraphEdge$new, element = self, simplify = FALSE)
    },

    print = function(...) {
      BBmisc::catf("GraphNode: <%s>", self$pipeop$id)
    }
  ),
  private = list(
      .next_nodes = NULL,
      .prev_nodes = NULL,
      .editlock = FALSE,
      .in_edges = NULL,
      .out_edges = NULL,
      .graph = NULL
  ),
  active = list(
      graph = function() private$.graph
      prev_nodes = function(prev) {
        if (!missing(prev)) {
          connectgn(prev, ".prev_nodes", private)
        }
        private$.prev_nodes
      },
      next_nodes = function(nxt) {
        if (!missing(nxt)) {
          connectgn(nxt, ".next_nodes", private)
        }
        private$.next_nodes
      },
      in_edges = function() private$.in_edges,
      out_edges = function() private$.out_edges,

    input_complete = function() !any(sapply(self$prev_nodes, is.null)),
    output_complete = function() !any(sapply(self$next_nodes, is.null))
  )
)

connectgn = function(newnodes, oldnodename, private) {
  # TODO: assert prev is a list
  if (!identical(names(newnodes), names(private[[oldnodename]]))) {
    stop("Can't change names of nodes")
  }
  if (private$.editlock) return(NULL)
  backup = private[[oldnodename]]
  on.exit({private$.editlock = FALSE ; private[[oldnodename]] = backup})
  private$.editlock = TRUE
  for (idx in seq_along(newnodes)) {
    if (!identical(newnodes[[idx]]$element$graph, private$.graph)) {
      stop("Can't connect nodes that are not in the same graph")
    }
    oldnode = private[[oldnodename]][[idx]]
    edgename = names(newnodes)[[idx]]
    if (edgename == "") edgename = idx
    #TODO: assert class prev[[idx]]: NULL or GraphEdge
    if (identical(newnodes[[idx]], oldnode)) {
      next
    }
    oldnode$element$next_nodes[[oldnode$name]] = NULL
    newnodes[[idx]]$element$next_nodes[[newnodes[[idx]]$name]] = GraphEdge(edgename, self)
  }
  private[[oldnodename]] = newnodes
  private$.editlock = FALSE
  on.exit()
  # TODO: update graph in / out nodes
  # TODO: check types
}

#' ListNamedEls for GraphNodes.
#'
#' @description
#' It is used mainly inside the GraphNode class
#' to store the next and previous nodes.
#'
#' It's not exported.
#'
#' @noRd
#'
GraphNodesList = R6Class("GraphNodesList",
   inherit = ListNamedEls,

   public = list(
     initialize = function(xs = list()) {
       super$initialize(xs, "GraphNode", get_key = function(x) x$pipeop$id)
     },
     set_next = function(nodes) {
       nodes = wrap_nodes(nodes)
       self$map(function(x) x$set_next(nodes))
       GraphNodesList$new(nodes)
     }
   )
)

