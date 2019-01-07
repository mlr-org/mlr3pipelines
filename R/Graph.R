#' @include utils.R
#' @title Graph
#' @format [R6Class] Graph
#'
#' @description
#' The graph is a container class for the complete computational graph. It is made up of a list of
#' (connected) GraphNodes, it can be trained and used for prediction.
#'
#' @section Public Members / Active Bindings:
#' * `param_set`        ::  [ParamSet] \cr
#'   Set of all exposed parameters of the graph, a union of all `param_set` objects of all contained [PipeOp] objects.
#'   Parameter IDs are prefixed by PipeOp ID. Returns a deep-copy of all param sets.
#' * `param_vals`         ::  named `list`
#'   Set of all configured parameters of the graph, a union of all `param_vals` objects of all contained [PipeOp] objects. Parameter IDs are prefixed by PipeOp ID.
#' * `packages`         :: `character`
#'   Set of all required packages of the graph, a union of all required packages of all contained [PipeOp] objects.
#' * `node_list`      :: list of [GraphNode]` \cr
#'   Contains all nodes contained in the Graph, topologically sorted.
#' * `is_trained`     :: `logical(1)` \cr
#'   Is the graph, are all of its PipeOps, fully trained - and is the graph ready to predict?
#' * `intype`         :: `list` \cr
#'   Types of the `in_channels`, identifies types for the ingoing channels of nodes that are not connected yet.
#' * `outtype`        :: `list of any` \cr
#'   Types of the `out_channels`, identifies types for the outgoing channels of nodes that are not connected yet.
#' * `in_channels`    :: `list of [NodeChannel]` \cr
#'   Incoming NodeChannels of nodes that are not connected yet.
#' * `out_channels`   :: `list of [NodeChannel]` \cr
#'   Outgoing NodeChannels of nodes that are not connected yet.
#' * `lhs`   ::  list of [GraphNode]` \cr
#'   The 'left-hand-side' nodes that have some unconnected input channels and therefore act as graph input layer.
#' * `rhs`     :: `list of [GraphNode]` \cr
#'   The 'right-hand-side' nodes that have some unconnected output channels and therefore act as graph output layer.
#'
#' @section Methods:
#' * `new(fill = NULL)` \cr
#'   ([Graph]` | `NULL` | [PipeOp]) -> [Graph]
#'   Constructs an empty Graph, copies an existing graph if `fill` is a graph, or fills graph
#'   with node(s) if `fill` is a PipeOp. `fill` can also be a list of multiple Graphs / PipeOps.
#' * `f$add_node(node)` \cr
#'   (`[GraphNode] | [PipeOp]`) -> [Graph]
#'   Mutates graph by adding a [PipeOp] or [GraphNode] to the end of the graph.
#' * `extend(g)` \cr
#'   ([Graph] | PipeOp | list f [Graph]) -> `self`
#'   Add to the current graph per disjoint union.
#' * `f$map(fnc, simplify)` \cr
#'   `function`, `logical` -> 'list'
#'   Maps function over all graph nodes, returns a list, named by [PipeOp] ids.
#' * `f$train()`
#'   Train graph on its inputs, ensure that after that all nodes are trained and that an output list is present.
#' * `f$predict()`
#'    Predict graph on its inputs, ensure that after that an output list is present.
#' * `f$plot()`
#'    Plot the graph, via igraph.
#' * `f$print()`
#'   Print a minimal representaion of graph on console.
#' * get_pipeop
#'   list of [GraphNode], indexed by ID.
#' * `f[[`: Get a PipeOp by `[[id]]`
#' @name Graph
#' @family Graph
Graph = R6Class("Graph",
  cloneable = FALSE,
  public = list(
    initialize = function(fill = NULL) {
      self$update_connections()
      self$extend(fill)
    },

    # must be called when a member node's PipeOp-ID was changed.
    update_ids = function() {
      names(private$.node_list) = map_chr(private$.node_list, function(x) x$pipeop$id)
    },

    add_node = function(node) {
      if (!inherits(node, "GraphNode")) {
        # FIXME: assert node inherits PipeOp
        GraphNode$new(node, self)
      } else {
        assert(identical(node$graph, self))
        assert_null(private$.node_list[[node$pipeop$id]])
        private$.node_list[[node$pipeop$id]] = node
        self$update_connections()
      }
      invisible(self)
    },

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

    extend = function(src) {
      assert(
          check_class(src, "Graph", null.ok = TRUE),
          check_class(src, "PipeOp", null.ok = TRUE),
          check_class(src, "list", null.ok = TRUE)
      )

      # if src is a list, call self recursively
      if (inherits(src, "list")) {
        for (el in src) {
          self$extend(el)
        }
        return(invisible(self))
      }

      if (inherits(src, "PipeOp")) {
        return(self$add_node(src))
      }

      # add nodes: easy

      for (node in src$node_list) {
        self$add_node(node$pipeop$clone(deep = TRUE))
      }

      # replicate connections: harder
      for (nodename in names(src$node_list)) {
        oldnode = src$node_list[[nodename]]
        newnode = self$node_list[[nodename]]
        for (idx in seq_along(newnode$outtype)) {
          # for every channel in the node in the old graph we create the respective channel in the new graph
          oldchannel = oldnode$next_node_channels[[idx]]
          if (is.null(oldchannel)) next
          newchannel = self$node_list[[oldchannel$node$pipeop$id]]$in_channels[[oldchannel$channel_id]]
          newnode$next_node_channels[[idx]] = newchannel
        }
      }
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

    map = function(fnc, simplify = TRUE) {  # map over every node in the graph
      sapply(self$node_list, fnc, simplify = simplify)
    }
  ),

  active = list(
    node_list = readonly("node_list"),
    intype = function() private$.intype,
    outtype = function() private$.outtype,
    in_channels = readonly("in_channels"),
    out_channels = readonly("out_channels"),
    lhs = function() {
      source_ids = unique(map_chr(self$in_channels, function(edge) edge$node$pipeop$id))
      self$node_list[source_ids]
    },
    rhs = function() {
      sink_ids = unique(map_chr(self$out_channels, function(edge) edge$node$pipeop$id))
      self$node_list[sink_ids]
    },
    is_trained = function() all(self$map(function(x) x$pipeop$is_trained)), # all contained POs trained?
    param_set = function() {
      # loop over all nodes, and add their paramsets (with prefix) to result object
      ps = ParamSet$new()
      for (node in self$node_list) {
        prefix = paste0(node$pipeop$id, ".")
        ps2 = node$pipeop$param_set$clone(deep = TRUE)
        ps2$data$id = paste0(prefix, ps2$data$id)
        ps = ps$add_param_set(ps2) # FIXME: here the state in paradox of ps should change
      }
      return(ps)
    },
    param_vals = function(value) {
      if (!missing(value)) {
        parids = union_parids(self)  # collect all parameter ID mappings
        assert_list(value, names = "unique")  # length may not change
        assert(all(names(value) %in% names(parids)))
        if (!self$param_set$test(value)) {
          stop("Parameters out of bounds")
        }
        for (pidx in names(value)) {
          poid = parids[[pidx]][[1]]  # PipeOp ID of the PipeOp this pertains to
          parid = parids[[pidx]][[2]]  # original parameter id, as the PipeOp knows it
          self[[poid]]$pipeop$param_vals[[parid]] = value[[pidx]]
        }
      }
      union_parvals(self)
    },
    packages = function() unlist(unique(self$map(function(x) x$pipeop$packages)))
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

# Topological sort of nodes
# @param node_list [GraphNode] nodes in the same graph
# @param layerinfo [logical(1)] Whether to return a vector of graph depths instead of nodes
# @return the sorted `node_list` if `layerinfo` is FALSE. If `layerinfo` is TRUE, a named `numeric`
#   with node depths, indexed by node pipeop IDs, is returned.
# idea of code: for every node, we track how many unhandled prev nodes we have. only if this becomes 0,
# we can add the node to the toposort. if we do that, we substract each next_node counter by 1.
sort_nodes = function(node_list, layerinfo = FALSE) {
  numparents = function(node) sum(map_lgl(node$prev_nodes, Negate(is.null)))
  cache = map_dbl(node_list, numparents) # count how many connections to prev nodes a node has
  queue = names(cache)[cache == 0] # active queue for toposort, charvec if ids, we start with LHS nodes
  layers = sapply(queue, function(.) 0)
  cache = cache[cache != 0] # remove LHS nodes from cache
  queueidx = 1
  while (queueidx <= length(queue)) {
    current = queue[queueidx]
    nexts = table(map_chr(Filter(Negate(is.null), node_list[[current]]$next_nodes), function(n) n$pipeop$id))
    if (any(nexts %in% queue))
      stop("Graph is broken, this should never happen.")  # next_nodes and prev_nodes disagree about the graph structure.
    cache[names(nexts)] = cache[names(nexts)] - nexts # remove current next-node-counters from cache
    for (n in names(nexts))
      layers[n] = max(layers[n], layers[current] + 1)  # FIXME: bad code, we know when layer is corrent, this is when pending nodes become 0.
    queue = c(queue, names(cache)[cache == 0])    # add nodes with no prending prev nodes to end of queue
    cache = cache[cache != 0]      # only keep nodes with pending prev nodes
    queueidx = queueidx + 1
  }
  if (length(cache)) {
    stop("Cycles in graph!")
  }
  if (layerinfo) {
    sort(map_dbl(layers, identity))
  } else {
    node_list[queue]
  }
}


# Create the unified parameter values of all PipeOps inside a graph.
# The parameter IDs are changed by prefixing each PipeOp's ID, separated by a dot.
# @param graph [Graph]
# @return [named list]
union_parvals = function(graph) {
  parvals = unlist(graph$map(function(x) x$pipeop$param_vals, simplify = FALSE), recursive = FALSE)
  assert_list(parvals, names = "unique")
  parvals
}

# Create a mapping from a Graph's unified parameter IDs (the IDs given by `union_params`
# and `union_parvals`) to the actual PipeOp ID and its parameter ID.
#
# This is necessary because a global parameter ID of 'x.y.z' could pertain to a
# PipeOp 'x' with parameter 'y.z', or to a PipeOp 'x.y' with parameter 'z'.
# (In the first case, the returned value would be list(x.y.z = list('x', 'y.z')),
# in the second case it would be list(x.y.z = list('x.y', 'z')))
# @param graph [Graph]
# @return [list] list of pairs list(<original PipeOp ID>, <Parameter ID within that PipeOp>)
union_parids = function(graph) {
  parids = unlist(graph$map(function(x) {
    sapply(names(x$pipeop$param_vals), function(y) list(x$pipeop$id, y), simplify = FALSE)
  }, simplify = FALSE), recursive = FALSE)
  assert_list(parids, names = "unique")
  parids
}

# Successively call PipeOp train / predict functions while moving data along the node edges.
# @param input [any]: e.g. a task if the first node accepts a task input
# @param fncall [character(1)]: identifying a function to call for each edge. probably "train" or "predict"
# @param cache_result [logical(1)]: whether to store cached_output pipeop.
# @return [any] whatever the last node in the graph returns.
Graph$set("private", "reduceGraph", function(input, fncall, cache_result = FALSE) {

  sorted_nodes = self$node_list  # we rely on this to be topologically sorted, so we can just walk along this list

  in_channels = self$in_channels
  out_channels = self$out_channels
  if (length(in_channels) != 1) {
    stop("Graph has != 1 in_channels, not supported yet")
  }
  if (length(out_channels) != 1) {
    stop("Graph has != 1 out_channels, not supported yet")
  }

  startnode = which(names(sorted_nodes) == in_channels[[1]]$node$pipeop$id) # index in sorted_nodes
  stopnode = which(names(sorted_nodes) == out_channels[[1]]$node$pipeop$id) # index in sorted_nodes
  assert(length(startnode) == 1) ; assert(length(stopnode) == 1) ; assert(startnode <= stopnode)
  assert(identical(in_channels[[1]]$node, sorted_nodes[[startnode]]))
  assert(identical(out_channels[[1]]$node, sorted_nodes[[stopnode]]))
  assert(startnode == 1)  # don't support producers yet

  sorted_nodes = sorted_nodes[startnode:stopnode]

  inlist = list(input)
  names(inlist) = names(sorted_nodes[[1]]$intype)

  # the list of current inputs to some nodes
  # doubly indexed, 1st-index = node-id, 2nd-index = channel-id
  inputs = list()
  inputs[[names(sorted_nodes)[1]]] = inlist

  # go thru all nodes in toposorted order, call train/pred on them, store result
  for (node in sorted_nodes) {
    assert(node$pipeop$id %in% names(inputs))
    curin = inputs[[node$pipeop$id]]  # get input to current GN
    inputs[[node$pipeop$id]] = "00SENTINEL00"  # check later that we don't run in circles
    curout = node$pipeop[[fncall]](curin)   # work, with current input
    if (cache_result) node$pipeop$result = curout # store result in pipeop
    names(curout) = names(node$outtype)   # set the names of the output list # FIXME: this should not happen here
    assert(length(curout) == length(node$outtype)) # FIXME: this stuff should happen in pipeop, or graphnode


    # iterate thru all outchannels / elements of output-list
    # store element for connected-to node in the "inputs" structure
    for (idx in seq_along(node$outtype)) {
      outchannel = node$next_node_channels[[idx]]
      if (is.null(outchannel)) { # FIXME: why can an output channel be NULL...?
        assert(identical(node, tail(sorted_nodes, 1)[[1]]))
        next
      }
      nodename = outchannel$node$pipeop$id
      # if there is no element in inputs for the next node, create it
      if (nodename %nin% names(inputs)) {
        inputs[[nodename]] = sapply(outchannel$node$intype, function(.) NULL, simplify = FALSE) # FIXME: fugly init code..
      }
      assert(!identical(inputs[[nodename]], "00SENTINEL00"))
      inputs[[nodename]][[outchannel$channel_id]] = curout[[idx]]
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
  if (!any(map_lgl(private$.node_list, "editlock"))) {
    private$.node_list = sort_nodes(private$.node_list)
  }
  invisible(self)
})

# ----------------- plotting ----------------

graph_to_edge_list = function(nodes) {
  edges = map(nodes, function(x) {
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
