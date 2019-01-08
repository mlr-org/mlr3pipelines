Graph = R6Class("Graph",
  cloneable = FALSE,
  public = list(
    pipeops = NULL,
    channels = NULL,
    initialize = function() {
      self$pipeops = list()
      self$channels = data.table(
        src_id = character(0L),
        src_node = list(),
        src_channel = character(0L),
        dst_id = character(0L),
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
        src_id = src$id,
        src_node = list(src),
        src_channel = src_channel,
        dst = dst$id,
        dst_node = list(dst),
        dst_channel = dst_channel
      )
      self$channels = rbindlist(list(self$channels, row))
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
    }
  ),

  active = list(
    lhs = function() {
      setdiff(names(self$pipeops), self$channels[, unique(dst_id)])
    },

    rhs = function() {
      setdiff(names(self$pipeops), self$channels[, unique(src_id)])
    }
  )
)

if (FALSE) {
  devtools::load_all()
  self = g = Graph$new()

  op_pca = PipeOpPCA$new()
  g$add_pipeop(op_pca)
  g$plot()

  op_scale = PipeOpScale$new()
  g$add_pipeop(op_scale)
  g$plot()

  src_node = "pca"; src_channel = "1"; dst_node = "scale"; dst_channel = "1"
  g$add_channel("pca", "1", "scale", "1")
  g$channels

  op_lrn = PipeOpLearner$new(mlr_learners$get("classif.rpart"))
  g$add_pipeop(op_lrn)

  g$plot()

  g$lhs
  g$rhs
}
