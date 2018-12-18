graph_gather_params = function(root) {

  all_params = graph_map_topo(
    root,
    function(x) x$pipeop$param_set$clone(deep = TRUE)$params,
    simplify = FALSE
  )

  all_params_named = mapply(function(params_list, id) {

    lapply(params_list, function(x) {

      x = x$clone(deep = TRUE)
      x$id = paste(id, x$id, sep =":")
      x
    })

  }, all_params, names(all_params), SIMPLIFY = FALSE)

  paradox::ParamSet$new(params = unlist(all_params_named))
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

    source_nodes = list(),

    # FIXME: Do we need task_type and id?
    task_type = "classif",
    id = "foobar",

    # Do we clone/copy here? Otherwise state of OP's trained outside will change
    initialize = function(source_nodes) {
      # handles only GraphNode or list of the graph nodes
      if(inherits(source_nodes, "GraphNode")) {
        source_nodes = list(source_nodes)
      } else if(is.list(source_nodes)) {
        if(!all(vapply(source_nodes, TRUE, FUN = inherits, what = "GraphNode"))) {
          stop("Graph can only be initialized using GraphNode or a list of GraphNodes")
        }
      } else {
        stop("Only GraphNode or list of GraphNodes is supported.")
      }

      # Fixme: Should we do consitency checks (unique Id's etc here?)
      self$source_nodes = source_nodes
      self
    },

    # This should basically call trainGraph
    train = function(task) {
      trainGraph(self$source_nodes, task)
      invisible(self)
    },
    plot = function() {
      graph_plot(self$source_nodes)
      invisible(self)
    },

    # FIXME: the "state" of the coded pipeline is now in self and model. that seems weird?
    # can we remove "ops" from pipeline
    predict = function(task) {
      # FIXME: This should basically call the predict function on the GraphNodes
      nodes = self$map(function(x) x, simplify = FALSE) # get the nodes in topo order
      lapply(nodes, function(x) x$predict(task))
      invisible(self)
    },

    print = function(...) {
      res = graph_map_topo(self$source_nodes, add_layer = TRUE)
      res_string = tapply(
        res,
        attr(res, "layer"),
        FUN = function(x) paste(x, collapse = ",")
      )

      res_size = tapply(
        res,
        attr(res, "layer"),
        FUN = function(x) length(x)
      )

      output_string = paste(
        sprintf("[(%s), %s]", res_string, res_size),
        collapse = " >> "
      )
      output_string = base::strwrap(output_string, getOption("width") * 0.6)
      output_string = paste(paste(" ", output_string), collapse = "\n")
      cat("Pipeline Graph:\n")
      cat(output_string, "\n")
    },

    reset = function() {
      self$map(function(x) x$pipeop$reset(), simplify = FALSE)
      invisible(self)
    },

    map = function(fnc, simplify = TRUE) {
      graph_map_topo(self$source_nodes, fnc, simplify = simplify)
    },

    find_by_id = function(id) {
      # FIXME: We might want a version of traverseGraph that does this more efficiently.
      assert_choice(id, self$ids)
      nodes = self$map(function(x) {
        if(x$id == id) return(x)
      })
      nodes[[1]]
    }
  ),
  active = list(
    is_learnt = function(value) {
        all(self$map(function(x) x$is_learnt))
    },
    param_set = function(value) {
      if (missing(value)) graph_gather_params(self$source_nodes)
      },
    param_vals = function(value) {
      if (missing(value)) list()
      # FIXME: Allow setting Params for the Operators here
    },
    ids = function(value) {
      # FIXME: How are parallel Op's treated here?
      if (missing(value)) {
        self$map(function(x) x$id)
      } else {
        # FIXME: Should we allow overwriting id's here?
      }
    },
    packages = function() {
      pkgs = self$map(function(x) x$pipeop$packages)
      unique(pkgs)
    },
    lhs = function() { self$source_nodes },
    rhs = function() {
      self$map(function(x) {
        if(x$next_nodes$is_empty) return(x)
      })
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      if (name == "source_nodes") {
        print(value)
        map(value, function(x) x$clone(deep = TRUE))
      } else {
        value
      }
    }
  )
)

trainGraph = function(roots, task) {

  lapply(roots, function(x) x$inputs = list(task))
  front = GraphNodesList$new(roots)

  while(length(front) > 0L) {
    messagef("front step, front=%s", front$print_str)
    new_front = GraphNodesList$new()
    to_remove = integer(0L)
    for (i in seq_along(front)) {
      op = front[[i]]
      messagef("checking front node %s, can_fire=%s", op$id, op$can_fire)
      if (op$can_fire) {
        op$train()
        new_front$join_new(op$next_nodes)
      } else {
        new_front$add(op)
      }
    }
    front = new_front
    messagef("front step done, front=%s", front$print_str)
  }
}

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
  edges = graph_map_topo(root, simplify = FALSE, function(x) {
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

#' graph_map_topo
#'
#' @param root root node of the graph.
#' @param fnc function to apply
#' @param simplify should the result be simplified using simplify2array.
#' Default TRUE.
#' @param add_layer if true add information about the layer as a attribute.
#'
#' @return
#'
#' List (possibly simplified to vector) with the output
#' of function `fnc`  applied to all the nodes of the graph
#' in topological order. Note that only the output is in
#' topological order. The function `fnc` is not applied in that order,
#' contrary, the algorithm uses a `depth-first search`,
#' so it starts applying the function from the last element.
#'
#' @noRd
#'
graph_map_topo = function(roots, fnc = function(x) x$id, simplify = TRUE, add_layer = FALSE) {

  state = new.env()
  state$permanent = c()
  state$temporary = c()
  state$list = list()
  state$depth = numeric()

  visit = function(node, state, depth = 1) {
    if(node$id %in% state$permanent) {
      state$depth[node$id] = pmax(depth, state$depth[node$id], na.rm = TRUE)
      return()
    }
    if(node$id %in% state$temporary) stop("Not a DAG")
    state$temporary = c(node$id, state$temporary) # mark temporarily

    if(node$has_rhs) {
      res = lapply(
        node$next_nodes$xs,
        visit,
        state = state,
        depth = depth + 1
      )
    } else {
      res = NULL
    }
    state$permanent = c(node$id, state$permanent) # mark permanent
    state$temporary = state$temporary[node$id != state$temporary]

    state$list[[node$id]] = fnc(node)
    state$depth[node$id] = pmax(depth, state$depth[node$id], na.rm = TRUE)
  }

  lapply(roots, visit, state = state)

  state$depth = state$depth[names(state$list)]
  result      = state$list[order(state$depth)]
  state$depth = sort(state$depth)

  result = if(simplify) simplify2array(result) else result
  if(add_layer) attr(result, "layer") = state$depth
  result
}
