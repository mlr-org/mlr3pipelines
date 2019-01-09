
Graph = R6Class("Graph",
  public = list(
    pipeops = NULL,
    edges = NULL,

    # Initialize empty Graph
    initialize = function() {
      self$pipeops = list()
      self$edges = setDT(named_list(c("src_id", "src_channel", "dst_id", "dst_channel"), character()))
    },

    # Get IDs of all PipeOps. This is in order that PipeOps were added if
    # sorted is FALSE, and topologically sorted if sorted is TRUE.
    ids = function(sorted = FALSE) {
      if (!sorted || !nrow(self$edges))
        return(names2(self$pipeops))

      tmp = self$edges[, list(parents = list(src_id)), by = list(id = dst_id)]
      tmp = rbind(tmp, data.table(id = self$lhs, parents = list(character(0L))))
      topo_sort(tmp)$id
    },

    # Mutator that adds PipeOp 'op' to the Graph (without adding any edges)
    add_pipeop = function(op) {
      assert_class(op, "PipeOp")
      if (op$id %in% names(self$pipeops))
        stopf("PipeOp with id '%s' already in Graph", op$id)
      self$pipeops[[op$id]] = op$clone(deep = TRUE)
      invisible(self)
    },

    # add an edge from node src_id, and its channel src_channel, to
    # node dst_id's channel dst_channel
    add_edge = function(src_id, src_channel, dst_id, dst_channel) {
      assert_choice(src_id, names(self$pipeops))
      assert_choice(dst_id, names(self$pipeops))
      # FIXME: as soon as intypes / outtypes are present the following two lines should be:
      # assert_choice(src_channel, rownames(self$pipeops[[src_id]]$outtypes))
      # assert_choice(dst_channel, rownames(self$pipeops[[dst_id]]$intypes))
      assert_string(src_channel)
      assert_string(dst_channel)
      src_id_ = src_id
      dst_id_ = dst_id
      src_channel_ = src_channel
      dst_channel_ = dst_channel
      priorcon = self$edges[
        (src_id == src_id_ & src_channel == src_channel_) |
        (dst_id == dst_id_ & dst_channel == dst_channel_), ]
      if (nrow(priorcon)) {
        stopf("Cannot add multiple edges to a channel.\n%s",
          paste(sprintf("Channel %s of node %s already connected to node %s channel %s.",
            priorcon$src_id, priorcon$src_channel, priorcon$dst_id, priorcon$dst_channel), collapse = "\n"))
      }
      row = data.table(src_id = src_id, src_channel = src_channel,
        dst_id = dst_id, dst_channel = dst_channel)
      self$edges = rbind(self$edges, row)
    },

    # Plot the graph, using igraph
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

    # Print a representation of the graph
    # Output is currently a table of <id> <class of 'state'>
    print = function() {
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
      new_ids = map_values(names(self$pipeops), old, new)
      names(self$pipeops) = new_ids
      imap(self$pipeops, function(x, nn) x$id = nn)

      self$edges[, c("src_id", "dst_id") := list(map_values(src_id, old, new), map_values(dst_id, old, new))]
      invisible(self)
    },

    # Train graph by calling all the PipeOps' $train method
    # return a list of outputs for each unconnected PipeOp out-channel
    train = function(input) {
      graph_fire(self, private, input, "train")
    },

    # Predict with the graph by calling all the PipeOps' $predict method
    # return a list of outputs for each unconnected PipeOp out-channel
    predict = function(input) {
      graph_fire(self, private, input, "predict")
    }
  ),

  active = list(
    is_trained = function() all(map_lgl(self$pipeops, "is_trained")),
    lhs = function() sort(setdiff(names(self$pipeops), self$edges$dst_id)),
    rhs = function() sort(setdiff(names(self$pipeops), self$edges$src_id)),
    packages = function() unique(unlist(map(self$pipeops, "packages")))
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


graph_fire = function(self, private, input, stage) {
  assert_task(input)
  assert_choice(stage, c("train", "predict"))

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
    tmp = if (stage == "train") op$train(input) else op$predict(input)
    edges[get("src_id") == op$id, "result" := list(tmp)]
  }

  tmp
}
