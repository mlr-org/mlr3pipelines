#' @title Graph
#' @format [R6Class] Graph
#'
#' @description
#' The graph is a container class for the complete computational graph. It is made up of a list of
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
#' * `f$add_edge(src_id, src_channel, dst_id, dst_channel)` \cr
#'   (`character(1)`, `character(1)`, `character(1)`, `character(1)`) -> `self` \cr
#'   Add an edge from node `src_id`, and its channel `src_channel`, to node `dst_id`'s
#'   channel `dst_channel`.
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

    add_edge = function(src_id, src_channel, dst_id, dst_channel) {
      assert_choice(src_id, names(self$pipeops))
      assert_choice(dst_id, names(self$pipeops))
      # FIXME: as soon as intypes / outtypes are present the following two lines should be:
      # assert_choice(src_channel, rownames(self$pipeops[[src_id]]$outtypes))
      # assert_choice(dst_channel, rownames(self$pipeops[[dst_id]]$intypes))
      # FIXME: make these assert better? we cannot use arbitrary names
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
          paste(sprintf("Channel %s of node %s already connected to channel %s of node %s.",
            priorcon$src_channel, priorcon$src_id, priorcon$dst_channel, priorcon$dst_id), collapse = "\n"))
      }
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
      # print table <id>, <state>, where <state> is class(pipeop$state)
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

    train = function(input) {
      graph_fire(self, private, input, "train") # input assert in call
    },

    predict = function(input) {
      graph_fire(self, private, input, "predict") # input assert in call
    }
  ),

  active = list(
    is_trained = function() all(map_lgl(self$pipeops, "is_trained")),
    lhs = function() sort(setdiff(names(self$pipeops), self$edges$dst_id)),
    rhs = function() sort(setdiff(names(self$pipeops), self$edges$src_id)),
    packages = function() unique(unlist(map(self$pipeops, "packages"))),
    param_vals = function(rhs) {
      union_param_vals(map(self$pipeops, "param_set"), self$pipeops, "param_vals", rhs)
    },
    param_set = function() {
      union_param_sets(map(self$pipeops, "param_set"))
    },
    hash = function() {
      # FIXME: how do we depend on digest?
      # FIXME: maybe some pipeops need to tell us more about themselves than just ID (implicitly in map()), class, and param_vals?
      digest::digest(
        list(map(self$pipeops, class), self$param_vals, self$pipeops),
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
    },
    .hash = NA_character_
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
    tmp = if (stage == "train") op$train_internal(input) else op$predict_internal(input)
    edges[get("src_id") == op$id, "result" := list(tmp)]
  }

  filter_noop(tmp)
}

