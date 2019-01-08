GraphDT = R6Class("Graph",
  cloneable = FALSE,
  public = list(
    pipeops = NULL,
    channels = NULL,
    initialize = function() {
      self$pipeops = list()
      self$channels = data.table(
        src_id = character(0L),
        src_channel = character(0L),
        dst_id = character(0L),
        dst_channel = character(0L)
      )
    },

    add_pipeop = function(op) {
      assert_class(op, "PipeOp")
      if (op$id %in% names(self$pipeops))
        stopf("PipeOp with id '%s' already in Graph", op$id)
      self$pipeops = c(self$pipeops, set_names(list(op$clone(deep = TRUE)), op$id))
      invisible(self)
    },

    add_channel = function(src_id, src_channel, dst_id, dst_channel) {
      assert_choice(src_id, names(self$pipeops))
      assert_string(src_channel)
      assert_choice(dst_id, names(self$pipeops))
      assert_string(dst_channel)

      src = self$pipeops[[src_id]]
      dst = self$pipeops[[dst_id]]
      row = data.table(src_id = src_id, src_channel = src_channel,
        dst_id = dst_id, dst_channel = dst_channel)
      self$channels = rbind(self$channels, row)
    },

    plot = function() {
      require_namespaces("igraph")
      if (nrow(self$channels) == 0L) {
        ig = igraph::make_empty_graph()
        extra_vertices = names(self$pipeops)
      } else {
        df = self$channels[, list(from = src_id, to = dst_id)]
        ig = igraph::graph_from_data_frame(df)
        extra_vertices = setdiff(names(self$pipeops), c(df$from, df$to))
      }
      ig = igraph::add_vertices(ig, length(extra_vertices), name = extra_vertices)
      layout = igraph::layout_with_sugiyama(ig)$layout
      if (!is.matrix(layout))
        layout = t(layout) # bug in igraph, dimension is dropped
      plot(ig, layout = layout)
    },

    # trains or predicts, depending on stage
    fire = function(input, stage) {
      assert_list(input)
      assert_choice(stage, c("train", "predict"))

      # add virtual channel to "__init__" in private copy of "channels"
      channels = copy(self$channels)
      channels = rbind(channels, data.table(src_id = "__init__", src_channel = "1",
        dst_id = self$lhs, dst_channel = "1"))

      # add new column to store results and store 'input' as result of virtual operator "__init__"
      channels$result = list()
      channels[src_id == "__init__", result := list(input)]

      # topo-sort the pipeop ids
      tmp = channels[, list(parents = list(src_id)), by = list(id = dst_id)]
      tmp = rbind(tmp, data.table(id = "__init__", parents = list(character(0L))))
      ids = setdiff(topo_sort(tmp)$id, "__init__")

      # walk over ids, learning each operator
      for (id in ids) {
        op = self$pipeops[[id]]
        input = channels[dst_id == op$id, "result"]$result
        tmp = if (stage == "train") op$train(input) else op$predict(input)
        channels[src_id == op$id, result := list(tmp)]
      }

      tmp
    }
  ),

  active = list(
    lhs = function() { # return OP?
      setdiff(names(self$pipeops), unique(self$channels$dst_id))
    },

    rhs = function() { # return OP?
      setdiff(names(self$pipeops), unique(self$channels$src_id))
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      switch(name,
        "channels" = copy(value),
        "pipeops" = map(value, function(x) x$clone(deep = TRUE)),
        value
      )
    }
  )
)
