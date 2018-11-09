sub#' Traverse graphs and apply the function `fnc`
traverseGraph <- function(root, fnc) {
  #FIXME: check visited nodes
  front = GraphNodesList$new(list(root))
  result_list <- list()

  while(length(front) > 0L) {
    new_front = GraphNodesList$new()
    for (i in seq_along(front)) {
      op = front[[i]]
      result_list[[op$id]] <- fnc(op)

      if(is.null(op$next_nodes)) break
      new_front$join_new(op$next_nodes)
    }
    front = new_front
  }
  result_list
}

graph_gather_params <- function(graph) {

  all_params <- traverseGraph(
    graph,
    function(x) x$pipeop$par_set$clone(deep = TRUE)$params
  )

  all_params_named <- mapply(function(params_list, id) {

    lapply(params_list, function(x) {

      x <- x$clone(deep = TRUE)
      x$id <- paste(id, x$id, sep =":")
      x
    })

  }, all_params, names(all_params), SIMPLIFY = FALSE)

  paradox::ParamSet$new(params = unlist(all_params_named))
}


trainGraph = function(root, task) {
  root$inputs = list(task)
  front = GraphNodesList$new(list(root))

  while(length(front) > 0L) {
    BBmisc::messagef("front step, front=%s", front$print_str)
    new_front = GraphNodesList$new()
    to_remove = integer(0L)
    for (i in seq_along(front)) {
      op = front[[i]]
      BBmisc::messagef("checking front node %s, can_fire=%s", op$id, op$can_fire)
      if (op$can_fire) {
        op$train()
        new_front$join_new(op$next_nodes)
      } else {
        new_front$add(op)
      }
    }
    front = new_front
    BBmisc::messagef("front step done, front=%s", front$print_str)
  }
}


# class Graph
# members:
#   source_node
#
# methods
# - index operator [[id]]  --> points to GraphNode
#
# active bindings:
#   - is_learnt [logical].  Are all underlying operators trained?
#   - parset [ParamSet]. Returns flat ParamSet, names are pipeOpid:parid, it is computed on the fly.
#   - parvals [list]. Set param vals, name scheme as above, passed them down to pipeOps via id.
#   - ids [character]. Id's of PipeOp's in the graph.
#
# questions:
#   - does index op also work with ints?
#   - do we want the fourth layer of topological sorting?
#   - how do we loops over all nodes? how do we apply something to all nodes?
Graph = R6Class("Graph",

  public = list(

    source_node = list(),
    packages = character(0L),

    # FIXME: Do we need task_type and id?
    task_type = "classif",
    id = "foobar",

    # Do we clone/copy here? Otherwise state of OP's trained outside will change
    initialize = function(source_node) {
      # Fixme: Should we do consitency checks (unique Id's etc here?)
      self$source_node = source_node
    },

    # This should basically call trainGraph
    train = function(task) {
      trainGraph(self$source_node, task)
    },

    # FIXME: the "state" of the coded pipeline is now in self and model. that seems weird?
    # can we remove "ops" from pipeline
    predict = function(task) {
      # FIXME: This should basically call the predict function on the GraphNodes
    },

    print = function(...) {
      s = self$ids
      s = BBmisc::collapse(s, "->")
      BBmisc::catf("Graph: %s", s)
    },

    reset = function() {
      # FIXME: This should reset all PipeOp's in the graph
    },

    find_by_id = function(id) {
      # FIXME: We might want a version of traverseGraph that does this more efficiently.
      assert_choice(id, self$ids)
      nodes = traverseGraph(self$source_node, function(x) {
        if(x$id == id) {
          return(x)
        } else {
          return(NULL)
        }
      })
      nodes[[1]]
    }
  ),
  active = list(
    is_learnt = function(value) {
      ifelse(
        all(unlist(traverseGraph(self$source_node, function(x) x$is_learnt))),
        TRUE,
        FALSE)
    },
    par_set = function(value) {
      if (missing(value)) graph_gather_params(self$source_node)
      },
    par_vals = function(value) {
      if (missing(value)) list()
      # FIXME: Allow setting Params for the Operators here
    },
    ids = function(value) {
      # FIXME: How are parallel Op's treated here?
      if (missing(value)) {
        unlist(traverseGraph(self$source_node, function(x) x$id))
      } else {
        # FIXME: Should we allow overwriting id's here?
      }
    }
  )
)

#' @export
length.Graph = function(x) {
  length(x$ids)
}

#' @export
`[[.Graph` = function(x, i, j, ...) {
  if (is.character(i)) {
    x$find_by_id(i)
  } else if (is.integer(i)) {
    # FIXME: This will break for parallel operators.
    x$find_by_id(x$ids[i])
  }
}

graph_to_edge_list = function(root) {
  edges = traverseGraph(root, function(x) {
    res = cbind(

      x$id,
      x$next_nodes$map(function(y) y$id)
    )
    if(ncol(res) > 1) res
  })
  edges = do.call(rbind, edges)
  mode(edges) = "character"
  rownames(edges) = NULL
  edges
}

graph_plot = function(root) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Please install package 'igraph'")
  }

  edges = graph_to_edge_list(root)
  g = igraph::graph_from_edgelist(edges)
  layout =  igraph::layout_with_sugiyama(g)

  plot(g, layout = layout$layout)
}
