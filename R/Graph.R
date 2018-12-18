
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
  inherit = GraphElement,
  public = list(

    initialize = function() {
      self$update_connections()
      self
    },

    update_connections = function() {  # update intype, outtype, inputs, outputs
      private$.in_edges = list()
      private$.out_edges = list()
      private$.intype = list()
      private$.outtype = list()
      for (node in self$node_list) {
        assigncon = function(edges, types, nodes, edgetarget, typetarget) {
          for (conidx in seq_along(edges)) {
            if (is.null(nodes[[conidx]])) {
              edgename = names2(edges)[[conidx]]
              if (is.na(edgename)) {
                edgename = length(private[[edgetarget]]) + 1
              }
              private[[edgetarget]][[edgename]] = edges[[conidx]]
              private[[typetarget]][[edgename]] = types[[conidx]]
            }
          }
        }
        assigncon(node$in_edges, node$intype, node$next_nodes, ".in_edges", ".intype")
        assigncon(node$out_edges, node$outtype, node$prev_nodes, ".out_edges", ".outtype")
      }
    },

    add_node = function(node) {
      if (!inherits(node, "GraphNode")) {
        # TODO: assert node inherits PipeOp
        node = GraphNode(node, self)
      }
      assert(identical(node$graph, self))
      assert_null(private$.node_list[[node$pipeop$id]])
      private$.node_list[[node$pipeop$id]] = node
      self$update_connections()
    },

    # This should basically call trainGraph
    train = function(task) {
      trainGraph(self$source_nodes, task)
      invisible(self)
    },
    plot = function() {
      graph_plot(self$node_list)
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
      if (!length(self$node_list)) return(cat("Empty Graph.\n"))

      layers = sort_nodes(self$node_list, TRUE)
      res_string = tapply(
        names(layers),
        layers,
        FUN = paste,
        collapse = ","
      )

      res_size = tapply(
        names(layers),
        layers,
        FUN = length
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
        sort_nodes(self$node_list)
      },
      intype = function() private$.intype,
      outtype = function() private$.outtype,
      in_edges = function() private$.in_edges,
      out_edges = function() private$.out_edges,

      source_nodes = function() {
        source_ids = unique(map_chr(self$in_edges, function(edge) edge$element$pipeop$id))
        self$node_list[source_ids]
      },
      sink_nodes = function() {
        sink_ids = unique(map_chr(self$out_edges, function(edge) edge$element$pipeop$id))
        self$node_list[sink_ids]
      },
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
  )
)




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

#' @export
`[[<-.Graph` = function(x, i, j, value) {
  if (!identical(x$node_list[[i]], value)) {
    stop("Cannot re-assign graph nodes")
  }
  x
}

graph_to_edge_list = function(nodes) {
  edges = map(sort_nodes(nodes), function(x) {
    res = cbind(
      x$pipeop$id,
      map_chr(Filter(Negate(is.null), x$next_nodes), function(y) y$pipeop$id)
    )
    if(ncol(res) > 1) res
  })
  edges = do.call(rbind, edges)
  mode(edges) = "character"
  rownames(edges) = NULL
  edges
}

graph_plot = function(nodes) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Please install package 'igraph'")
  }

  edges = graph_to_edge_list(nodes)
  if (length(edges)) {
    g = igraph::graph_from_edgelist(edges, directed = TRUE)
  } else {
    g = igraph::make_empty_graph()
  }

  forgotten = setdiff(names(nodes), edges)
  g = igraph::add_vertices(g, length(forgotten), name = forgotten)
  layout = igraph::layout_with_sugiyama(g)
  plot(g, layout = layout$layout)
}


sort_nodes = function(node_list, layerinfo = FALSE) {
  pending = function(node) sum(map_lgl(node$prev_nodes, Negate(is.null)))
  cache = map_dbl(node_list, pending)
  queue = names(cache)[cache == 0]
  layers = sapply(queue, function(.) 0, simplify = FALSE)
  cache = cache[cache != 0]
  queueidx = 1
  while (queueidx <= length(queue)) {
    current = queue[queueidx]
    nexts = table(map_chr(Filter(Negate(is.null), node_list[[current]]$next_nodes), function(n) n$pipeop$id))
    if (any(nexts %in% queue)) stop("Cycles in graph!")
    cache[names(nexts)] = cache[names(nexts)] - nexts
    for (n in names(nexts)) layers[[n]] = min(layers[[n]], layers[[current]] + 1)
    queue = c(queue, names(cache)[cache == 0])
    cache = cache[cache != 0]
    queueidx = queueidx + 1
  }
  if (length(cache)) {
    stop("Unconnected nodes found")
  }
  if (layerinfo) {
    sort(map_dbl(layers, identity))
  } else {
    node_list[queue]
  }
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

