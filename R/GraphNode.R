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

##### Methods definitions #####
# set_next
graph_node_set_next = function(nodes) {

  nodes = wrap_nodes(nodes)
  self$next_nodes = GraphNodesList$new(nodes)
  for(nn in nodes) {
    nn$add_prev(self)
  }

  self$next_nodes
}

# set_prev
graph_node_set_prev = function(nodes) {

  nodes = wrap_nodes(nodes)
  self$prev_nodes = GraphNodesList$new(nodes)
  for(nn in nodes) {
    nn$add_next(self)
  }
  self$prev_nodes
}


graph_node_add_next = function(nodes) {
  nodes = wrap_nodes(nodes)
  self$next_nodes$join_new(GraphNodesList$new(nodes))
  self
}

graph_node_add_prev = function(nodes) {
  nodes = wrap_nodes(nodes)
  self$prev_nodes$join_new(GraphNodesList$new(nodes))
  self
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
#' @usage GraphNode
#'
#' @importFrom R6 R6Class
#'
#'
GraphNode = R6::R6Class(
  "GraphNode",
  public = list(
    initialize = function(pipeop) {
      self$pipeop = pipeop
      self$next_nodes = GraphNodesList$new()
      self$prev_nodes = GraphNodesList$new()
    },
    pipeop = NULL,
    inputs = list(),

    # next, prev
    next_nodes = NULL,
    prev_nodes = NULL,
    set_next = graph_node_set_next,
    set_prev = graph_node_set_prev,
    add_next = graph_node_add_next,
    add_prev = graph_node_add_prev,
    train = function() {
      if (self$has_lhs) {
        self$inputs = self$prev_nodes$map(function(x) x$result)
      }
      messagef("Train op='%s'", self$id)
      self$pipeop$train(self$inputs)
    },
    next_node = function(id = 1) self$next_nodes[[id]],

    print = function(...) {
      catf("GraphNode: <%s>", self$id)
    }
  ),
  active = list(

    # forwarded
    id = function(id) {
      if (missing(id)) {
        self$pipeop$id
      } else {
        self$pipeop$id = id
      }} ,
    result = function() self$pipeop$result,
    has_result = function() !is.null(self$pipeop$result),
    is_learnt  = function() self$pipeop$is_learnt,

    has_lhs = function() length(self$prev_nodes) > 0L,
    has_rhs = function() length(self$next_nodes) > 0L,
    can_fire = function() {
      if (!self$has_lhs) length(self$inputs) > 0
      else all(self$prev_nodes$map_s(function(x) x$has_result))
    },
    root_node = function() {
      if(self$has_no_prevs) return(self)
      self$prev_nodes[[1]]$root_node
    }
  )
)

wrap_nodes = function(x) {

  check_node = function(y) {
    checkmate::assert_class(y, "GraphNode")
    y
  }

  if(inherits(x, "GraphNode")) {
    x = list(check_node(x))
  } else if(is.list(x)) {
    x = lapply(x, check_node)
  }
  x
}
