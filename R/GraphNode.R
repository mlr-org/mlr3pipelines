GraphNodesList = R6Class("GraphNodesList",
   inherit = ListNamedEls,
   
   public = list(
     initialize = function(xs = list()) {
       super$initialize(xs, "GraphNode", get_key = function(x) x$pipeop$id)
     }
   )
)

##### Methods definitions #####
# set_next
graph_node_set_next <- function(nodes) {
  nodes <- wrap_pipeops(nodes)
  self$next_nodes = GraphNodesList$new(nodes)
  for(nn in nodes) {
    nn$add_prev(self)
  }
  
  self
}

# set_prev
graph_node_set_prev <- function(nodes) {
  nodes <- wrap_pipeops(nodes)
  self$prev_nodes = GraphNodesList$new(nodes)
  
  for(nn in nodes) {
    nn$add_next(self)
  }
  self
}


graph_node_add_next <- function(nodes) {
  nodes <- wrap_pipeops(nodes)
  self$next_nodes$join_new(GraphNodesList$new(nodes))
  self
}

graph_node_add_prev <- function(nodes) {
  nodes <- wrap_pipeops(nodes)
  self$prev_nodes$join_new(GraphNodesList$new(nodes))
  self
}


#### Class definition ####
GraphNode = R6::R6Class(
  "GraphNode", 
  public = list(
    initialize = function(pipeop) {
      self$pipeop <- pipeop 
      self$next_nodes <- GraphNodesList$new()
      self$prev_nodes <- GraphNodesList$new()
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
      if (!self$has_no_prevs) {
        self$inputs = self$prev_nodes$map(function(x) x$result)
      }
      BBmisc::messagef("Train op='%s'", self$id)
      self$pipeop$train(self$inputs)
    },
    next_node = function(id = 1) self$next_nodes[[id]],
    
    print = function(...) {
      BBmisc::catf("GraphNode: <%s>", self$id)
    }
  ),
  private = list(),
  active = list(
    
    # forwarded
    id = function() self$pipeop$id,
    result = function() self$pipeop$result,
    has_result = function() !is.null(self$pipeop$result),
    is_learnt  = function() self$pipeop$is_learnt,
    
    has_no_prevs = function() length(self$prev_nodes) == 0L,
    can_fire = function() {
      if (self$has_no_prevs) !is.null(self$inputs)
      else all(self$prev_nodes$map_s(function(x) x$has_result))
    },
    root_node = function() {
      if(self$has_no_prevs) return(self)
      self$prev_nodes[[1]]$root_node
    }
  )
)

#' Automatically transform PipeOp or a list of PipeOps into a list of GraphNodes
#'
#' @param x
#' 
#' @return  
#' 
#' If \code{x} is a GraphNode the function returns the list(x).
#'
#' @noRd
#'
wrap_pipeops <- function(x) {
  
  wrap_pipeop <- function(y) {
    checkmate::assert_multi_class(y, c("PipeOp", "GraphNode"))
    if(inherits(y, "PipeOp")) {
      y <- GraphNode$new(y)
    }
    y
  }
  
  if(inherits(x, "PipeOp") || inherits(x, "GraphNode")) {
    x <- list(wrap_pipeop(x))
  } else if(is.list(x)) {
    x <- lapply(x, wrap_pipeop)
  }
  
  x
}
