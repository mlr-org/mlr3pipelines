
# TODO:
# * print()
Graph = R6Class("Graph",
  public = list(
    pipeops = NULL,
    edges = NULL,
    initialize = function() {
      self$pipeops = list()
      self$edges = setDT(named_list(c("src_id", "src_channel", "dst_id", "dst_channel"), character()))
    },

    ids = function(sorted = FALSE) {
      if (!sorted || !nrow(self$edges))
        return(names2(self$pipeops))

      tmp = self$edges[, list(parents = list(src_id)), by = list(id = dst_id)]
      tmp = rbind(tmp, data.table(id = self$lhs, parents = list(character(0L))))
      topo_sort(tmp)$id
    },

    add_pipeop = function(op) {
      assert_class(op, "PipeOp")
      if (op$id %in% names(self$pipeops))
        stopf("PipeOp with id '%s' already in Graph", op$id)
      self$pipeops[[op$id]] = op$clone(deep = TRUE)
      invisible(self)
    },

    add_channel = function(src_id, src_channel, dst_id, dst_channel) {
      assert_choice(src_id, names(self$pipeops))
      assert_choice(dst_id, names(self$pipeops))
      # FIXME: as soon as intypes / outtypes are present the following two lines should be:
      # assert_choice(src_channel, rownames(self$pipeops[[src_id]]$outtypes))
      # assert_choice(dst_channel, rownames(self$pipeops[[dst_id]]$intypes))
      assert_string(src_channel)
      assert_string(dst_channel)

      row = data.table(src_id = src_id, src_channel = src_channel,
        dst_id = dst_id, dst_channel = dst_channel)
      self$edges = rbind(self$edges, row)
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
      lines = map(self$pipeops[self$ids(sorted = TRUE)], function(pipeop) {
        data.frame(ID = pipeop$id, State = sprintf("<%s>", class(pipeop$state)[1]))
      })
      if (length(lines)) {
        catf("Graph with %s PipeOps:", length(lines))
        print(do.call(rbind, unname(lines)))
      } else {
        cat("Empty Graph.\n")
      }
      invisible(self)
    },

    set_names = function(old, new) {
      new_ids = map_values(names(self$pipeops), old, new)
      names(self$pipeops) = new_ids
      imap(self$pipeops, function(x, nn) x$id = nn)

      self$edges[, c("src_id", "dst_id") := list(map_values(src_id, old, new), map_values(dst_id, old, new))]
      invisible(self)
    },

    # FIXME: why does this take a list of inputs? in in "graph_fire" the arg is called "input"?
    # shouldnt this always be a single task?
    train = function(inputs) {
      graph_fire(self, private, inputs, "train")
    },

    predict = function(inputs) {
      graph_fire(self, private, inputs, "predict")
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


graph_fire = function(self, private, inputs, stage) {
  assert_list(inputs, types = "Task")
  assert_choice(stage, c("train", "predict"))

  # add virtual channel to "__init__" in private copy of "edges"
  edges = copy(self$edges)
  edges = rbind(edges, data.table(src_id = "__init__", src_channel = "1",
      dst_id = self$lhs, dst_channel = "1"))

  # add new column to store results and store 'inputs' as result of virtual operator "__init__"
  edges$result = list()
  edges[get("src_id") == "__init__", "result" := list(inputs)]

  # get the topo-sorted the pipeop ids
  ids = setdiff(self$ids(sorted = TRUE), "__init__")

  # walk over ids, learning each operator
  for (id in ids) {
    op = self$pipeops[[id]]

    inputs = edges[get("dst_id") == op$id, "result"][[1L]]
    tmp = if (stage == "train") op$train(inputs) else op$predict(inputs)
    edges[get("src_id") == op$id, "result" := list(tmp)]
  }

  tmp
}
