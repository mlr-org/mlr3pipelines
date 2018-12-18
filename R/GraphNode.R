
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
      initialize = function(name, element) {
        self$name = name
        self$element = element
      }
  )
)

numbername = function(li) lapply(seq_along(li), function(x) {
  ret = names2(li)[x]
  if (is.na(ret)) x else ret
})

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
      private$.next_nodes = sapply(pipeop$intype, function(.) NULL, simplify = FALSE)
      private$.prev_nodes = sapply(pipeop$outtype, function(.) NULL, simplify = FALSE)
      private$.in_edges = lapply(numbername(pipeop$intype), GraphEdge$new, element = self)
      names(private$.in_edges) = names(pipeop$intype)
      private$.out_edges = lapply(numbername(pipeop$outtype), GraphEdge$new, element = self)
      names(private$.out_edges) = names(pipeop$outtype)
      graph$add_node(self)
      self
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
      .graph = NULL,
      .pipeop = NULL
  ),
  active = list(
      graph = function() private$.graph,
      pipeop = function() private$.pipeop,
      prev_nodes = function(prev) {
        if (!missing(prev)) {
          connectgn(prev, ".prev_nodes", self, private)
        }
        private$.prev_nodes
      },
      next_nodes = function(nxt) {
        if (!missing(nxt)) {
          connectgn(nxt, ".next_nodes", self, private)
        }
        private$.next_nodes
      },
      in_edges = function() private$.in_edges,
      out_edges = function() private$.out_edges,
      intype = function() self$pipeop$intype,
      outtype = function() self$pipeop$outtype,
    input_complete = function() !any(sapply(self$prev_nodes, is.null)),
    output_complete = function() !any(sapply(self$next_nodes, is.null))
  )
)

connectgn = function(newnodes, oldnodename, self, private) {
  # TODO: assert prev is a list
  if (!identical(names(newnodes), names(private[[oldnodename]]))) {
    stop("Can't change names of nodes")
  }
  if (private$.editlock) return(NULL)
  backup = private[[oldnodename]]
  on.exit({private$.editlock = FALSE ; private[[oldnodename]] = backup})
  private$.editlock = TRUE
  for (idx in seq_along(newnodes)) {
    if (!identical(newnodes[[idx]]$graph, private$.graph)) {
      stop("Can't connect nodes that are not in the same graph")
    }
    oldnode = private[[oldnodename]][[idx]]
    edgename = names(newnodes)[[idx]]
    if (is.null(edgename) || edgename == "") edgename = idx
    #TODO: assert class prev[[idx]]: NULL or GraphEdge
    if (identical(newnodes[[idx]], oldnode)) {
      next
    }
    oldnode$element$next_nodes[[oldnode$name]] = NULL
    newnodes[[idx]]$next_nodes[[newnodes[[idx]]$name]] = GraphEdge$new(edgename, self)
  }
  private[[oldnodename]] = newnodes
  private$.editlock = FALSE
  on.exit()
  self$graph$update_connections()
  # TODO: check types
}
