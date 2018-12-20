# FIXME
#   - does index op also work with ints?
#   - do we want the fourth layer of topological sorting?
#   - how do we loops over all nodes? how do we apply something to all nodes?
#   - ids [character]. Id's of PipeOp's in the graph.

#' @include utils.R
#' @title Graph
#' @format [R6Class] Graph
#' @description
#'   The graph is a container class for the complete computational graph. It is made up of a list of
#'   (connected) GraphNodes, it can be trained and used for prediction.
#' @section Usage:
#' * `f = Graph$new(copy = NULL)` \cr
#' *  `[Graph]` | `NULL`-> [Graph]
#' * `f$node_list` -> `list of [GraphNode]`
#' * `f$sorted_node_list` -> `list of [GraphNode]`
#' * `f$is_trained` -> `logical(1)`
#' * `f$intype` -> `list of any`
#' * `f$outtype` -> `list of any`
#' * `f$in_channels` -> `list of [NodeChannel]`
#' * `f$out_channels` -> `list of [NodeChannel]`
#' * `f$source_nodes` -> `list of [GraphNode]`
#' * `f$sink_nodes` -> `list of [GraphNode]`
#' * `param_set` -> [ParamSet] The set of all exposed parameters of the PipeOp.
#' * `par_vals` -> `named list`
#' * `f$add_node(node = pipeOp$new())` \cr
#' *  `[GraphNode] | [PipeOp]` -> [Graph]
#' * `f$extend(g)` \cr
#' *  `[Graph]` -> `[Graph]`
#' * `f$map(fnc, simplify)` \cr
#' *  `function`, `logical` -> 'list of any`
#' * `f$train()`
#' * `f$predict()`
#' * `f$plot()`
#' * `f$print()`
#' * `f$update_connections()`
#' * `f$update_ids()`
#' * `f[[` -> `[PipeOp]`
#'
#' Aggregated info:
#' * `param_set` [ParamSet]
#' * `param_vals` [list]
#' * `packages` [character]
#'
#' @section Details:
#' - `new()`: Constructs an empty Graph, or copies an existing graph if `copy` is a graph.
#' * `node_list`: list of [GraphNode], indexed by ID.
#' * `sorted_node_list`: like `node_list`, but ordered by connections.
#' * `f[[`: Get a PipeOp by `[[id]]`
#' * `intype`: types of the `in_channels`.
#' * `outtype`: types of the `out_channels`.
#' * `source_nodes`: nodes that have unconnected input channels and therefore act as graph input.
#' * `sink_nodes`: nodes that have unconnected output channels and therefore act as graph output.
#' * `add_node`: Mutates graph by adding a [PipeOp] or [GraphNode] to the end of the graph.
#'   [GraphNode] calls this automatically on construction, so a user should only call this with a PipeOp.
#' * `train()`: train on input (e.g. Task), returns processed output (e.g. modified task).
#' * `predict()`: predict on input (e.g. Data), get processed output (e.g. predictions).
#' * `plot()`: plot the graph.
#' * `extend(g)`: Add other graph `g` to the current graph as disjoint union.
#' @name Graph
#' @family Graph
#' @examples
#' # Initialize a new GraphNode
#' g = Graph$new()
#' # Add a new node / pipeOp
#' g$add_node(PipeOp$new())
Graph = R6Class("Graph",
  cloneable = FALSE,
  public = list(
    initialize = function(copy = NULL) {
      assert_class(copy, "Graph", null.ok = TRUE)
      self$update_connections()
      if (!is.null(copy)) {
        self$extend(copy)
      }
      return(self)
    },
    update_ids = function() {
      names(private$.node_list) = map_chr(private$.node_list, function(x) x$pipeop$id)
    },
    add_node = function(node) {
      if (!inherits(node, "GraphNode")) {
        # TODO: assert node inherits PipeOp
        GraphNode$new(node, self)
      } else {
        assert(identical(node$graph, self))
        assert_null(private$.node_list[[node$pipeop$id]])
        private$.node_list[[node$pipeop$id]] = node
        self$update_connections()
      }
      self
    },
    # This should basically call trainGraph
    train = function(task) {
      private$reduceGraph(task, "train", TRUE)
    },
    predict = function(task) {
      private$reduceGraph(task, "predict", TRUE)
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
          if (is.null(oldchannel)) next
          newchannel = self$node_list[[oldchannel$node$pipeop$id]]$in_channels[[oldchannel$name]]
          newnode$next_node_channels[[idx]] = newchannel
        }
      }
      return(self)
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
    node_list = readonly("node_list"),
    sorted_node_list = function() sort_nodes(self$node_list),
    intype = function() private$.intype,
    outtype = function() private$.outtype,
    in_channels = readonly("in_channels"),
    out_channels = readonly("out_channels"),
    source_nodes = function() {
      source_ids = unique(map_chr(self$in_channels, function(edge) edge$node$pipeop$id))
      self$node_list[source_ids]
    },
    sink_nodes = function() {
      sink_ids = unique(map_chr(self$out_channels, function(edge) edge$node$pipeop$id))
      self$node_list[sink_ids]
    },
    is_trained = function(value) all(self$map(function(x) x$pipeop$is_trained)),
    param_set = function() union_params(self),
    param_vals = function(value) {
      if (missing(value)) list()
    },
    packages = function() unique(self$map(function(x) x$pipeop$packages)),
    lhs = function() self$source_nodes,
    rhs = function() self$sink_nodes
  ),
  private = list(
      .node_list = list(),
      .intype = NULL,
      .outtype = NULL,
      .in_channels = NULL,
      .out_channels = NULL
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
    for (n in names(nexts)) layers[[n]] = max(layers[[n]], layers[[current]] + 1)
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

union_params = function(graph) {
  ps = ParamSet$new()
  graph$map(function(x) {
    prefix = x$pipeop$id
    xps = x$pipeop$param_set
    newps = ParamSet$new(lapply(xps$get_params(), function(x) { x$data$id = paste(prefix, x$id, sep = ".") ; x}))
    ps$add_param_set(newps)
  })
  ps
}

# input: e.g. task
# fncall: character(1) identifying a function to call for each edge. probably "train" or "predict"
# cache_result: whether to store cached_output pipeop
Graph$set("private", "reduceGraph", function(input, fncall, cache_result = FALSE) {
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
    if (cache_result) node$pipeop$result = curout
    if (!node$pipeop$returnslist) {
      assert(length(node$outtype) == 1)
      curout = list(curout)
      names(curout) = names(node$outtype)
    }
    assert(length(curout) == length(node$outtype))

    for (idx in seq_along(node$outtype)) {
      outchannel = node$next_node_channels[[idx]]
      if (is.null(outchannel)) {
        assert(identical(node, tail(sorted_nodes, 1)[[1]]))
        next
      }
      nodename = outchannel$node$pipeop$id
      if (nodename %nin% names(inputs)) {
        inputs[[nodename]] = sapply(outchannel$node$intype, function(.) NULL, simplify = FALSE)
      }
      assert(!identical(inputs[[nodename]], "00SENTINEL00"))
      inputs[[nodename]][[outchannel$name]] = curout[[idx]]
    }
  }
  assert(length(curout) == 1)
  curout[[1]]
})

Graph$set("public", "update_connections", function() {  # update intype, outtype, inputs, outputs
  private$.in_channels = list()
  private$.out_channels = list()
  private$.intype = list()
  private$.outtype = list()
  assigncon = function(channels, types, otherchan, edgetarget, typetarget) {
    for (conidx in seq_along(channels)) {
      if (is.null(otherchan[[conidx]])) {
        edgename = names2(channels)[[conidx]]
        if (is.na(edgename)) {
          edgename = length(private[[edgetarget]]) + 1
        }
        private[[edgetarget]][[edgename]] = channels[[conidx]]
        private[[typetarget]][[edgename]] = types[[conidx]]
      }
    }
  }

  for (node in self$node_list) {
    assigncon(node$in_channels, node$intype, node$prev_node_channels, ".in_channels", ".intype")
    assigncon(node$out_channels, node$outtype, node$next_node_channels, ".out_channels", ".outtype")
  }
})

# ----------------- plotting ----------------

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
