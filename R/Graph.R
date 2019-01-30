#' @title Graph
#' @format [R6Class] Graph
#'
#' @description
#' `Graph` is a container class for the complete computational graph. It is made up of a list of
#' PipeOps, and a [`data.table`] of edges. It can be trained and used for prediction.
#'
#' @section Public Members / Active Bindings:
#' * `pipeops`      :: named list of [PipeOp] \cr
#'   Contains all PipeOps contained in the Graph, named by the PipeOp `$id`.
#' * `edges`        :: [`data.table`] \cr
#'   List of connections between the PipeOps. A `data.table` with columns `src_id`, `src_channel`,
#'   `dst_id`, `dst_channel`. `src_id` and `dst_id` are IDs of PipeOps that must be present in
#'   the `pipeops` list. `src_channel` and `dst_channel` must be channel IDs of the respective PipeOps.
#' * `is_trained`   :: `logical(1)` \cr
#'   Is the graph, are all of its PipeOps, fully trained - and is the graph ready to predict?
#' * `lhs`          ::  list of [PipeOp]` \cr
#'   The 'left-hand-side' nodes that have some unconnected input channels and therefore act as graph input layer.
#' * `rhs`          :: `list of [PipeOp]` \cr
#'   The 'right-hand-side' nodes that have some unconnected output channels and therefore act as graph output layer.
#' * `packages`     :: `character`
#'   Set of all required packages of the graph, a union of all required packages of all contained [PipeOp] objects.
#'
#' @section Methods:
#' * `Graph$new()` \cr
#'   Constructs an empty Graph.
#' * `f$ids(sorted = FALSE)` \cr
#'   `logical(0)` -> `character` \cr
#'   Get IDs of all PipeOps. This is in order that PipeOps were added if
#'   `sorted` is `FALSE`, and topologically sorted if `sorted` is `TRUE`.
#' * `f$add_pipeop(op)` \cr
#'   ([`PipeOp`]) -> [Graph] \cr
#'   Mutates graph by adding a [PipeOp] to the graph (without adding any edges)
#' * `f$add_edge(src_id, dst_id, src_channel, dst_channel)` \cr
#'   (`character(1)`, `character(1)`,
#'   `character(1)` | `numeric(1)` | `NULL`,
#'   `character(1)` | `numeric(1)` | `NULL`) -> `self` \cr
#'   Add an edge from node `src_id`, and its channel `src_channel`
#'   (identified by its name or line number in the node's `$output`), to node `dst_id`'s
#'   channel `dst_channel` (identified by its name or line number in the node's `$input`).
#'   If source or destination node have only one input / output channel and `src_channel` / `dst_channel`
#'   are therefore unambiguous they can be omitted (left as `NULL`).
#' * `f$plot()` \cr
#'   Plot the graph, via igraph.
#' * `f$print()` \cr
#'   Print a representation of the graph on the console. Output is currently a table with columns `id`, and
#'   short representation of `state`.
#' * `f$set_names(old, new)` \cr
#'   (`character`, `character`) -> `self` \cr
#'   Rename PipeOps: Change ID of each PipeOp as identified by `old` to the corresponding item in `new`.
#' * `f$train()` \cr
#'   [`Task`] -> `list` of any \cr
#'   Train graph by calling all the PipeOps' $train method. Return a list of outputs for each unconnected
#'   PipeOp out-channel. During training, the `$state` member of the PipeOps will be set.
#' * `f$predict()` \cr
#'   [`Task`] -> `list` of any \cr
#'   Predict with the graph by calling all the PipeOps' $predict method. Return a list of outputs for each
#'   unconnected PipeOp out-channel
#' @name Graph
#' @export
Graph = R6Class("Graph",
  public = list(
    pipeops = NULL,
    edges = NULL,

    initialize = function() {
      self$pipeops = list()
      self$edges = setDT(named_list(c("src_id", "src_channel", "dst_id", "dst_channel"), character()))
    },

    ids = function(sorted = FALSE) {
      assert_flag(sorted)
      if (!sorted || !nrow(self$edges))
        return(names2(self$pipeops))

      tmp = self$edges[, list(parents = list(src_id)), by = list(id = dst_id)]
      tmp = rbind(tmp, data.table(id = self$lhs, parents = list(character(0L))))
      topo_sort(tmp)$id
    },

    add_pipeop = function(op) {
      assert_r6(op, "PipeOp")
      if (op$id %in% names(self$pipeops))
        stopf("PipeOp with id '%s' already in Graph", op$id)
      self$pipeops[[op$id]] = op$clone(deep = TRUE)
      invisible(self)
    },

    add_edge = function(src_id, dst_id, src_channel = NULL, dst_channel = NULL) {
      assert_choice(src_id, names(self$pipeops))
      assert_choice(dst_id, names(self$pipeops))
      if (is.null(src_channel)) {
        if (length(self$pipeops[[src_id]]$output$name) > 1) {
          stopf("src_channel must not be NULL if src_id pipeop has more than one output channel.")
        }
        src_channel = 1
      }
      if (is.null(dst_channel)) {
        if (length(self$pipeops[[dst_id]]$input$name) > 1) {
          stopf("src_channel must not be NULL if src_id pipeop has more than one output channel.")
        }
        dst_channel = 1
      }
      assert(
          check_integerish(src_channel, lower = 1,
            upper = nrow(self$pipeops[[src_id]]$output), any.missing = FALSE),
          check_choice(src_channel, self$pipeops[[src_id]]$output$name)
      )
      if (is.numeric(src_channel))
        src_channel = self$pipeops[[src_id]]$output$name[src_channel]
      assert(
          check_integerish(dst_channel, lower = 1,
            upper = nrow(self$pipeops[[dst_id]]$input), any.missing = FALSE),
          check_choice(dst_channel, self$pipeops[[dst_id]]$input$name)
      )
      if (is.numeric(dst_channel))
        dst_channel = self$pipeops[[dst_id]]$input$name[dst_channel]

      badrows = self$edges$src_id == src_id & self$edges$src_channel == src_channel |
        self$edges$dst_id == dst_id & self$edges$dst_channel == dst_channel
      if (any(badrows)) {
        priorcon = self$edges[badrows]
        stopf("Cannot add multiple edges to a channel.\n%s",
          paste(sprintf("Channel %s of node %s already connected to channel %s of node %s.",
            priorcon$src_channel, priorcon$src_id, priorcon$dst_channel, priorcon$dst_id), collapse = "\n"))
      }
      row = data.table(src_id, src_channel, dst_id, dst_channel)
      oldedges = self$edges
      self$edges = rbind(self$edges, row)
      # check for loops
      on.exit({self$edges = oldedges})
      self$ids(sorted = TRUE)  # if we fail here, edges get reset.
      on.exit()
      invisible(self)
    },

    plot = function() {
      require_namespaces("igraph")
      if (nrow(self$edges) == 0L) {
        ig = igraph::make_empty_graph()
        extra_vertices = names(self$pipeops)
      } else {
        df = self$edges[, list(from = src_id, to = dst_id)]
        ig = igraph::graph_from_data_frame(df)
        extra_vertices = setdiff(names(self$pipeops), c(df$from, df$to))
      }
      ig = igraph::add_vertices(ig, length(extra_vertices), name = extra_vertices)
      layout = igraph::layout_with_sugiyama(ig)$layout
      if (!is.matrix(layout))
        layout = t(layout) # bug in igraph, dimension is dropped
      plot(ig, layout = layout)
    },

    print = function() {
      # print table <id>, <state>, where <state> is `class(pipeop$state)`
      lines = map(self$pipeops[self$ids(sorted = TRUE)], function(pipeop) {
        data.frame(ID = pipeop$id, State = sprintf("<%s>",
          map_values(class(pipeop$state)[1], "NULL", "<UNTRAINED>")))
      })
      if (length(lines)) {
        catf("Graph with %s PipeOps:", length(lines))
        print(do.call(rbind, unname(lines)))
      } else {
        cat("Empty Graph.\n")
      }
      invisible(self)
    },

    # Mutator to change PipeOp IDs
    # Modifies both the index in $pipeops, as well as the respective PipeOp's ID. Do this here and not
    # by setting `graph$pipeops[[x]]$id <- y`!
    set_names = function(old, new) {
      ids = names(self$pipeops)
      assert_subset(old, ids)
      assert_character(new, any.missing = FALSE)
      new_ids = map_values(ids, old, new)
      names(self$pipeops) = new_ids
      imap(self$pipeops, function(x, nn) x$id = nn)

      self$edges[, c("src_id", "dst_id") := list(map_values(src_id, old, new), map_values(dst_id, old, new))]
      invisible(self)
    },

    train = function(input, single_input = TRUE) {
      graph_fire(self, private, input, "train", single_input)
    },

    predict = function(input, single_input = TRUE) {
      graph_fire(self, private, input, "predict", single_input)
    }
  ),

  active = list(
    is_trained = function() all(map_lgl(self$pipeops, "is_trained")),
    lhs = function() unique(self$input$op.id),
    rhs = function() unique(self$output$op.id),
    input = function() {
      graph_channels(self$edges$dst_id, self$edges$dst_channel, self$pipeops, "input")
    },
    output = function() {
      graph_channels(self$edges$src_id, self$edges$src_channel, self$pipeops, "output")
    },
    packages = function() unique(unlist(map(self$pipeops, "packages"))),
    param_vals = function(rhs) {
      union_param_vals(map(self$pipeops, "param_set"), self$pipeops, "param_vals", rhs)
    },
    param_set = function() {
      union_param_sets(map(self$pipeops, "param_set"))
    },
    hash = function() {
      digest(
        list(map(self$pipeops, "hash"), self$edges),
        algo = "xxhash64")
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      switch(name,
        edges = copy(value),
        pipeops = map(value, function(x) x$clone(deep = TRUE)),
        value
      )
    }
  )
)

# build the '$input' or '$output' active binding
# ids, channels: columns of $edges DT. Either src_{ids,channels} or dst_{ids,channels}, depending on if we are input or output
# pipeops: list of pipeops
# direction: "input" or "output"
graph_channels = function(ids, channels, pipeops, direction) {
  if (!length(pipeops)) {
    return(data.table(name = character(0), train = character(0),
      predict = character(0), op.id = character(0), channel.name = character(0)))
  }
  rbindlist(lapply(pipeops, function(po) {
    # Note: This uses data.frame and is 20% faster than the fastest data.table I could come up with
    # (and factor 2 faster than a naive data.table implementation below).
    # $input and $output is actually a bottleneck for %>>%, so we want this to be fast.
    # Please don't change without benchmark.
    df = as.data.frame(po[[direction]], stringsAsFactors = FALSE)
    rows = df$name %nin% channels[ids == po$id]
    if (!any(rows)) {
      return(data.frame(name = character(0),
        train = character(0), predict = character(0),
        op.id = character(0), channel.name = character(0),
        stringsAsFactors = FALSE))
    }
    df$op.id = po$id
    df = df[rows,
      c("name", "train", "predict", "op.id", "name")]
    df[[1]] = paste0(po$id, ".", df[[1]])
    names(df)[5] = "channel.name"
    df
  }))
}

graph_channels_dt = function(ids, channels, pipeops, direction) {
  # for reference: this is the above in data.table
  if (!length(pipeops)) {
    return(data.table(name = character(0), train = character(0),
      predict = character(0), op.id = character(0), channel.name = character(0)))
  }
  rbindlist(lapply(pipeops, function(po) {
    po[[direction]][name %nin% channels[ids == po$id],
      list(name = paste0(po$id, ".", name),
        train = train, predict = predict, op.id = po$id, channel.name = name)]
  }))
}

graph_fire = function(self, private, input, stage, single_input) {
  assert_flag(single_input)
  assert_choice(stage, c("train", "predict"))

  edges = copy(self$edges)

  graph_input = self$input
  graph_output = self$output

  if (!single_input) {
    # input can be a named list (will be distributed to respective edges) or unnamed.
    # if it is named, we check that names are unambiguous.
    assert_list(input, len = nrow(graph_input))
    if (!is.null(names(input))) {
#      if (anyDuplicated(graph_input$name
    }
  }

  # add virtual channel to "__init__" in private copy of "edges"
  edges = copy(self$edges)
  edges = rbind(edges, data.table(src_id = "__init__", src_channel = "1",
      dst_id = self$lhs, dst_channel = "1"))

  # add new column to store results and store 'input' as result of virtual operator "__init__"
  edges$result = list()
  edges[get("src_id") == "__init__", "result" := list(list(input))]

  # get the topo-sorted the pipeop ids
  ids = setdiff(self$ids(sorted = TRUE), "__init__")

  # walk over ids, learning each operator
  for (id in ids) {
    op = self$pipeops[[id]]
    input = edges[get("dst_id") == op$id, "result"][[1L]]
    tmp = if (stage == "train") op$train_internal(input) else op$predict_internal(input)
    #FIXME: why do we use "get" here? also used in other places!
    # FIXME: how can we ever be sure that the results here are EVER written to the correct channehls
    # in the correct order....?
    edges[get("src_id") == op$id, "result" := list(tmp)]
  }
  # FIXME: actually we SHOUKD store the intermediate results in the pipeop itself, much easier
  filter_noop(tmp)
}

