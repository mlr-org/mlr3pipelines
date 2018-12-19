


# input: e.g. task
# fncall: character(1) identifying a function to call for each edge. probably "train" or "predict"
# cache_result: whether to store cached_output pipeop
reduceGraph = function(input, fncall, cache_result = FALSE) {

  sorted_nodes = self$sorted_node_list
  in_channels = self$in_channels
  out_channels = self$out_channels
  if (length(in_channels) != 1) {
    stop("Graph has != 1 in_channels, not supported yet")
  }
  if (length(out_channels) != 1) {
    stop("Graph has != 1 out_channels, not supported yet")
  }
  startnode = which(names(sorted_nodes) == in_channels[[1]]$node$pipeop$id)
  stopnode = which(names(sorted_nodes) == out_channels[[1]]$node$pipeop$id)
  assert(length(startnode) == 1) ; assert(length(stopnode) == 1) ; assert(startnode <= stopnode)
  assert(identical(in_channels[[1]]$node, sorted_nodes[[startnode]]))
  assert(identical(out_channels[[1]]$node, sorted_nodes[[stopnode]]))
  assert(startnode == 1)  # don't support producers yet

  sorted_nodes = sorted_nodes[startnode:stopnode]

  inlist = list(input)
  names(inlist) = names(sorted_nodes[[1]]$intype)

  inputs = list()
  inputs[[names(sorted_nodes)[1]]] = inlist

  for (node in sorted_nodes) {
    assert(node$pipeop$id %in% names(inputs))
    curin = inputs[[node$pipeop$id]]
    inputs[[node$pipeop$id]] = "00SENTINEL00"  # check later that we don't run in circles
    if (!node$pipeop$takeslist) {
      assert(length(curin) == 1)
      curin = curin[[1]]
    }
    curout = node$pipeop[[fncall]](curin)
    if (cache_result) node$pipeop$cached_output = curout
    if (!node$pipeop$returnslist) {
      assert(length(node$outtype) == 1)
      curout = list(curout)
      names(curout) = names(node$outtype)
    }
    assert(length(curout) == length(node$outtype))

    for (idx in seq_along(node$outtype)) {
      outchannel = node$next_node_channels[[idx]]
      nodename = outchannel$node$pipeop$id
      if (nodename %nin% names(inputs)) {
        inputs[[nodename]] = sapply(outchannel$node$intype, function(.) NULL, simplify = FALSE)
      }
      assert(!identical(inputs[[nodename]], "00SENTINEL00"))
      inputs[[nodename]][[outchannel$name]] = curout[[idx]]
    }
  }
  assret(length(curout) == 1)
  curout[[1]]
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
  cloneable = FALSE,
  public = list(

    initialize = function() {
      self$update_connections()
      self
    },

    update_connections = function() {  # update intype, outtype, inputs, outputs
      private$.in_channels = list()
      private$.out_channels = list()
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
        assigncon(node$in_channels, node$intype, node$next_nodes, ".in_channels", ".intype")
        assigncon(node$out_channels, node$outtype, node$prev_nodes, ".out_channels", ".outtype")
      }
    },
    update_ids = function() {
      names(private$.node_list) = map_chr(private$.node_list, function(x) x$pipeop$id)
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
      private$.reduceGraph(task, "train", TRUE)
    },
    predict = function(task) {
      private$.reduceGraph(task, "predict", TRUE)
    },
    plot = function() {
      graph_plot(self$node_list)
      invisible(self)
    },

    extend = function(graph) {
      # add nodes: easy
      for (node in graph$node_list) {
        self$add_node(node$pipeop)
      }

      # replicate connections: harder
      for (nodename in names(graph$node_list)) {
        oldnode = graph$node_list[[nodename]]
        newnode = self$node_list[[nodename]]
        for (idx in seq_along(newnode$outtype)) {
          oldchannel = oldnode$next_node_channels[[idx]]
          newchannel = self$node_list[[oldchannel$node$pipeop$id]]$in_channels[[oldchannel$name]]
          newnode$next_node_channels[[idx]] = newchannel
        }
      }
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
      in_channels = function() private$.in_channels,
      out_channels = function() private$.out_channels,

      source_nodes = function() {
        source_ids = unique(map_chr(self$in_channels, function(edge) edge$node$pipeop$id))
        self$node_list[source_ids]
      },
      sink_nodes = function() {
        sink_ids = unique(map_chr(self$out_channels, function(edge) edge$node$pipeop$id))
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
      .in_channels = NULL,
      .out_channels = NULL,
      .reduceGraph = reduceGraph
  )
)

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

