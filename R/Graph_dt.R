GraphDT = R6Class("Graph",
  cloneable = FALSE,
  public = list(
    pipeops = NULL,
    channels = NULL,
    initialize = function() {
      self$pipeops = list()
      self$channels = data.table(
        src_node = list(),
        src_channel = character(0L),
        dst_node = list(),
        dst_channel = character(0L)
      )
    },

    add_pipeop = function(op) {
      assert_class(op, "PipeOp")
      if (op$id %in% names(self$pipeops))
        stopf("PipeOp with id '%s' already in Graph", op$id)
      self$pipeops = c(self$pipeops, set_names(list(op), op$id))
      invisible(self)
    },

    add_channel = function(src_node, src_channel, dst_node, dst_channel) {
      assert_choice(src_node, names(self$pipeops))
      assert_string(src_channel)
      assert_choice(dst_node, names(self$pipeops))
      assert_string(dst_channel)

      src = self$pipeops[[src_node]]
      dst = self$pipeops[[dst_node]]
      row = data.table(
        src_node = list(src),
        src_channel = src_channel,
        dst_node = list(dst),
        dst_channel = dst_channel
      )
      self$channels = rbind(self$channels, row)
    },

    plot = function() {
      require_namespaces("igraph")
      if (nrow(self$channels) == 0L) {
        ig = igraph::make_empty_graph()
        extra_vertices = names(self$pipeops)
      } else {
        df = self$channels[, list(from = map_chr(src_node, "id"), to = map_chr(dst_node, "id"))]
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
      op_init = PipeOpNULL$new("__init__")
      channels = rbind(channels, data.table(src_node = list(op_init), src_channel = "1",
        dst_node = self$pipeops[self$lhs], dst_channel = "1"))

      # add helper columns
      channels[, c("src_id", "dst_id") := list(map_chr(src_node, "id"), map_chr(dst_node, "id"))]

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
      setdiff(names(self$pipeops), unique(map_chr(self$channels$dst_node, "id")))
    },

    rhs = function() { # return OP?
      setdiff(names(self$pipeops), unique(map_chr(self$channels$src_node, "id")))
    }
  )
)
