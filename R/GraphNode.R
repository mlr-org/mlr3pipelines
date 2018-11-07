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
  private$next_nodes = GraphNodesList$new(nodes)
  for(nn in nodes) {
    nn$add_prev(self)
  }
  
  self
}

# set_prev
graph_node_set_prev <- function(nodes) {
  nodes <- wrap_pipeops(nodes)
  private$prev_nodes = GraphNodesList$new(nodes)
  
  for(nn in nodes) {
    nn$add_next(self)
  }
  self
}


graph_node_add_next <- function(nodes) {
  nodes <- wrap_pipeops(nodes)
  private$next_nodes$join_new(GraphNodesList$new(nodes))
  self
}

graph_node_add_prev <- function(nodes) {
  nodes <- wrap_pipeops(nodes)
  private$prev_nodes$join_new(GraphNodesList$new(nodes))
  self
}


#### Class definition ####
GraphNode = R6::R6Class(
  "GraphNode", 
  public = list(
    initialize = function(pipeop) {
      self$pipeop <- pipeop 
      private$next_nodes <- GraphNodesList$new()
      private$prev_nodes <- GraphNodesList$new()
    },
    browser = function() browser(),
    pipeop = NULL,
    set_next = graph_node_set_next,
    set_prev = graph_node_set_prev,
    add_next = graph_node_add_next,
    add_prev = graph_node_add_prev,
    train = function(task) {},
    next_node = function(id = 1) private$next_nodes[[id]],
    
    print = function(...) {
      BBmisc::catf("GraphNode: <%s>", self$id)
    }
  ),
  private = list(
    next_nodes = NULL,
    prev_nodes = NULL,
    inputs = list(),
    acquire_inputs = function() {
      if (!self$has_no_prevs)
        self$inputs = private$prev_nodes$map(function(x) x$result)
    }
  ),
  active = list(
    id = function() self$pipeop$id,
    result = function() self$pipeop$result,
    has_result = function() !is.null(self$pipeop$result),
    has_no_prevs = function() length(private$prev_nodes) == 0L,
    can_fire = function() {
      if (self$has_no_prevs) !is.null(self$inputs)
      else all(self$prev_nodes$map_s(function(x) x$has_result))
    },
    prev_nodes_list = function() private$prev_nodes,
    next_nodes_list = function() private$next_nodes,
    root_node = function() {
      if(self$has_no_prevs) return(self)
      private$prev_nodes[[1]]$root_node
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
