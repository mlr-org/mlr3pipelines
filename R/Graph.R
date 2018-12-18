
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
  inherits = GraphElement,
  public = list(

    initialize = function() {
      update_connections()
      self
    },

    update_connections = function() {  # update intype, outtype, inputs, outputs
      private$.in_edges = list()
      private$.out_edges = list()
      private$.intype = list()
      private$.outtype = list()
      for (node in self$node_list) {
        assigncon(node$in_edges, node$intype, node$in_edges, ".in_edges", "intype", private)
        assigncon(node$out_edges, node$outtype, node$out_edges, ".out_edges", "outtype", private)
      }
    },

    add_node = function(node) {
      if (!inherits(node, "GraphNode")) {
        # TODO: assert node inherits PipeOp
        node = GraphNode(node, self)
      }
      assert_identical(node$graph, self)
      assert_null(private$.node_list[[node$pipeop$id]])
      private$.node_list[[node$pipeop$id]] = node
      update_connections()
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

    map = function(fnc, simplify = TRUE) {
      sapply(self$sorted_node_list, fnc, simplify = simplify)
    }
  ),
  active = list(
      node_list = function() private$.node_list,
      sorted_node_list = function() {
        queue = names(self$source_nodes)
        out = queue
        while (length(queue)) {
          current = queue[1]
          nexts = sapply(self$node_list[[current]]$out_edges, function(e) e$element$pipeop$id)
          nexts = setdiff(nexts, out)
          queue = c(queue[-1], nexts)
          out = c(out, nexts)
        }
        private$.node_list[[out]]
      },
      intype = function() private$.intype,
      outtype = function() private$.outtype,
      in_edges = function() private$.in_edges,
      out_edges = function() private$.out_edges,

      source_nodes = function() {
        source_ids = unique(sapply(self$in_edges, function(edge) edge$element$pipeop$id))
        self$node_list[source_ids]
      },
      sink_nodes = function() {
        sink_ids = unique(sapply(self$out_edges, function(edge) edge$element$pipeop$id))
        self$node_list[sink_ids]
      }
    is_learnt = function(value) {
        all(self$map(function(x) x$is_learnt))
    },
    param_set = function(value) {
      if (missing(value)) graph_gather_params(self$source_nodes)
      },
    param_vals = function(value) {
      if (missing(value)) list()
    },
    packages = function() {
      unique(self$map(function(x) x$pipeop$packages))
    },
    lhs = function() self$source_nodes,
    rhs = function() self$sink_nodes
  ),
  private = list(
      .node_list = list(),
      .intype = NULL,
      .outtype = NULL,
      .in_edges = NULL,
      .out_edges = NULL
  }
)

assigncon = function(edges, types, nodes, edgetarget, typetarget, private) {
  for (conidx in seq_along(edges)) {
    if (is.null(nodes[[conidx]])) {
      edgename = names(edges)[[conidx]]
      if (is.null(edgename) || edgename == "") {
        edgename = length(private[[edgetarget]]) + 1
      }
      private$[[edgetarget]][[edgename]] = edges[[conidx]]
      private$[[typetarget]][[edgename]] = types[[conidx]]
    }
  }
}



trainGraph = function(roots, task) {

  lapply(roots, function(x) x$inputs = list(task))
  front = GraphNodesList$new(roots)

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

#' @export
length.Graph = function(x) {
  length(x$node_list)
}

#' @export
`[[.Graph` = function(x, i, j, ...) {
  x$node_list[[i]]
}

graph_to_edge_list = function(root) {
  edges = root$map(simplify = FALSE, function(x) {
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

graph_gather_params = function(root) {

  all_params = root$map(
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
