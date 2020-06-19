#' @include mlr_graphs.R

#' @title Robustify a learner
#' @name mlr_graphs_robustify
#' @description
#' Creates a [`Graph`] that can be used to robustify any subsequent learner.
#' Performs the following steps:
#' * Drops empty factor levels using [`PipeOpFixFactors`]
#' * Imputes `numeric` features using [`PipeOpImputeHist`] and [`PipeOpMissInd`]
#' * Imputes `factor` features using [`PipeOpImputeNewlvl`]
#' * Encodes `factors` using `one-hot-encoding`. Factors with a cardinality > max_cardinality` are
#'   collapsed using [`PipeOpCollapseFactors`].
#' * If `scaling`, numeric features are scaled to mean 0 and standard deviation 1.
#'
#' The graph is built conservatively, i.e. the function always tries to assure everything works.
#' If a learner is provided, some steps can be left out, i.e. if the learner can deal with
#' factor variables, no encoding is performed.
#'
#' @param task [`Task`] \cr
#'   A [`Task`][mlr3::Task] to create a robustifying pipeline for.
#'   Optional, if omitted, the full pipeline is created.
#' @param learner [`Learner`][mlr3::Learner] \cr
#'   A learner to create a robustifying pipeline for. Optional, if omitted,
#'   a more conservative pipeline is built.
#' @param impute_missings `logical(1)` | `NULL` \cr
#'   Should missing values be imputed? Defaults to `NULL`, i.e imputes if the task has
#'   missing values and the learner can not handle them.
#' @param factors_to_numeric `logical(1)` | `NULL` \cr
#'   Should factors be encoded? Defaults to `NULL`, i.e encodes if the task has factors
#'   and the learner can not handle factors.
#' @param max_cardinality `integer(1)` \cr
#'   Maximum number of factor levels allowed. See above. Default: 1000.
#' @return [`Graph`]
#' @export
#' @examples
#' library(mlr3)
#' lrn = lrn("regr.rpart")
#' task = mlr_tasks$get("boston_housing")
#' gr = pipeline_robustify(task, lrn) %>>% po("learner", lrn)
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
pipeline_robustify = function(task = NULL, learner = NULL, impute_missings = NULL,
  factors_to_numeric = NULL, max_cardinality = 1000) {

  has_type_feats = function(types, if_null = TRUE) {
    if (is.null(task)) if_null else any(types %in% task$feature_types$type)
  }
  if (!is.null(task)) assert_task(task)
  if (!is.null(learner)) assert_learner(learner)
  if (is.null(impute_missings)) impute_missings = is.null(task) || (any(task$missings()) && (is.null(learner) || "missings" %nin% learner$properties))
  assert_flag(impute_missings)
  if (is.null(factors_to_numeric)) factors_to_numeric = is.null(task) || (has_type_feats("factor") && (is.null(learner) || "factors" %nin% learner$properties))
  assert_flag(impute_missings)
  assert_count(max_cardinality)

  # If given a task, only treat actually existing column types
  pos = list()

  # FIXME: Improve this when text-processors are available. See #332 and friends
  if (has_type_feats("character") && "character" %nin% learner$feature_types)
    pos = c(pos, po("colapply", id = "char_to_fct", param_vals = list(affect_columns = selector_type("character"), applicator = function(x) as.factor(x))))

  # Date processing
   if (has_type_feats("POSIXct") && ("POSIXct" %nin% learner$feature_types))
     pos = c(pos, po("datefeatures", param_vals = list(affect_columns = selector_type("POSIXct"))))

  if (impute_missings) {
    # Impute numerics
    if (has_type_feats(c("numeric", "integer")))
      pos = c(pos,
        gunion(list(
          po("imputehist"),
          po("missind", param_vals = list(affect_columns = selector_type(c("numeric", "integer")))))) %>>%
        po("featureunion"))
    # Impute factors
    if (has_type_feats(c("factor", "ordered", "character")))
      pos = c(pos, po("imputenewlvl"))
  }

  # Fix extra factor levels
  if (has_type_feats(c("factor", "ordered")))
    pos = c(pos, po("fixfactors"))

  # Ensure all factor levels are encoded during predict.
  if (impute_missings && has_type_feats(c("factor", "ordered", "character")))
    pos = c(pos, po("imputesample", affect_columns = selector_type(c("factor", "ordered", "character"))))

  # Collapse factors over 1000 levels
  # FIXME: Can be improved after #330 is solved
  if (is.null(task)) {
    pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
  } else {
    if (any(map_lgl(task$levels(task$feature_types$id[task$feature_types$type == "factor"]), function(x) length(x) > max_cardinality)))
      pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
  }

  if (factors_to_numeric) pos = c(pos, po("encode"))
  pos = c(pos, po("removeconstants"))
  as_graph(Reduce(`%>>%`, pos))
}

mlr_graphs$add("robustify", pipeline_robustify)


#' @title Create a bagging learner
#' @name mlr_graphs_bagging
#' @description
#' Creates a [`Graph`] that performs bagging for a supplied graph.
#' This is done as follows:
#' * `Subsample` the data in each step using [`PipeOpSubsample`], afterwards apply `graph`.
#' * Replicate this step `iterations` times (in parallel)
#' * Average outputs of replicated `graph`s predictions using the `averager`.
#'
#' @param graph [`PipeOp`] | [`Graph`] \cr
#'   A [`PipeOpLearner`] or [`Graph`] to create a robustifying pipeline for.
#'   Outputs from the replicated `graph`s are connected with the `averager`.
#' @param iterations `integer(1)` \cr
#'   Number of bagging iterations. Defaults to 10.
#' @param frac `numeric(1)` \cr
#'   Percentage of rows to keep during subsampling. See [`PipeOpSubsample`] for
#'   more information. Defaults to 0.7.
#' @param averager [`PipeOp`] | [`Graph`] \cr
#'   A [`PipeOp`] or [`Graph`] that averages the predictions from the
#'   replicated and subsampled graph's.
#'   In the simplest case, `po("classifavg")` and `po("regravg")` can be used
#'   in order to perform simple averaging of classification and regression
#'   predictions respectively.`
#'   If `NULL` (default), no averager is added to the end of the graph.
#' @return [`Graph`]
#' @export
#' @examples
#' library(mlr3)
#' lrn_po = po("learner", lrn("regr.rpart"))
#' task = mlr_tasks$get("boston_housing")
#' gr = pipeline_bagging(lrn_po, 3, averager = po("regravg"))
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
pipeline_bagging = function(graph, iterations = 10, frac = 0.7, averager = NULL) {
  assert_count(iterations)
  assert_number(frac, lower = 0, upper = 1)
  graph = as_graph(graph)
  if (!is.null(averager)) averager = as_graph(averager)

  subs = po("subsample", param_vals = list(frac = frac)) %>>% graph
  subs_repls = pipeline_greplicate(subs, iterations)

  subs_repls %>>% averager
}

mlr_graphs$add("bagging", pipeline_bagging)



#' @title Branch Between Alternative Paths
#' @name mlr_graphs_branching
#' @description
#' Create a multiplexed graph.
#'
#' @param graphs `list` of [`Graph`] \cr
#'   Multiple graphs, possibly named. They all must have exactly
#'   one output. If any of the arguments are named, then all must have
#'   unique names.
#' @param prefix_branchops `character(1)` \cr
#'   Optional id prefix to prepend to [`PipeOpBranch`] and [`PipeOpUnbranch`] id. Their
#'   resulting IDs will be `"[prefix_branchops]branch"` and `"[prefix_branchops]unbranch"`.
#'   Default is `""`.
#' @param prefix_paths `logical(1)` | `character(1)` \cr
#'   Whether to add prefixes to graph IDs when performing gunion. Can be helpful to
#'   avoid ID clashes in resulting graph. Default `FALSE`. If this is `TRUE`, the prefixes
#'   are taken from the names of the input arguments if present or `"poX"` where X counts up. If this is
#'   a `character(1)`, it is a prefix that is added to the `PipeOp` IDs *additionally*
#'   to the input argument list.
#'
#' @return [`Graph`]
#' @export
#' @examples
#' library("mlr3")
#'
#' po_pca = po("pca")
#' po_nop = po("nop")
#'
#' branches = pipeline_branch(list(pca = po_pca, nothing = po_nop))
#' # gives the same as
#' branches = c("pca", "nothing")
#' po("branch", branches) %>>%
#'   gunion(list(po_pca, po_nop)) %>>%
#'   po("unbranch", branches)
#'
#' pipeline_branch(list(pca = po_pca, nothing = po_nop),
#'   prefix_branchops = "br_", prefix_paths = "xy_")
#' # gives the same as
#' po("branch", branches, id = "br_branch") %>>%
#'   gunion(list(xy_pca = po_pca, xy_nothing = po_nop)) %>>%
#'   po("unbranch", branches, id = "br_unbranch")
pipeline_branch = function(graphs, prefix_branchops = "", prefix_paths = FALSE) {
  assert_list(graphs, null.ok = TRUE)
  assert_string(prefix_branchops)
  assert(
    check_flag(prefix_paths),
    check_string(prefix_paths)
  )
  assert(
    check_list(graphs, min.len = 1, any.missing = FALSE, names = "unique"),
    check_list(graphs, min.len = 1, any.missing = FALSE, names = "unnamed")
  )

  graphs = lapply(graphs, as_graph)
  imap(graphs, function(g, idx) {
    if (nrow(g$output) != 1) {
      stopf("Graph %s must have exactly one output channel", idx)
    }
  })

  graphs_input = graphs

  branches = if (is.null(names(graphs))) length(graphs) else names(graphs)
  if (!isFALSE(prefix_paths)) {
    if (is.null(names(graphs))) {
      names(graphs) = paste0("po", as.character(seq_along(graphs)))
    }
    if (is.character(prefix_paths)) {
      names(graphs) = paste0(prefix_paths, names(graphs))
    }
    poname_prefix = paste0(names(graphs), ".")
  } else {
    names(graphs) = NULL
    poname_prefix = ""
  }

  graph = gunion(list(graphs)) %>>% PipeOpUnbranch$new(branches, id = paste0(prefix_branchops, "unbranch"))

  branch_id = paste0(prefix_branchops, "branch")
  po_branch = PipeOpBranch$new(branches, id = branch_id)
  graph$add_pipeop(po_branch)

  pmap(list(graphs, poname_prefix, po_branch$output$name), function(gr, pnp, branch_chan) {
    gin = gr$input
    gin$op.id = paste0(pnp, gin$op.id)

    pmap(list(
      src_id = branch_id, dst_id = gin$op.id,
      src_channel = branch_chan, dst_channel = gin$channel.name),
      graph$add_edge)
  })
  graph
}

mlr_graphs$add("branch", pipeline_branch)

#' @title Transform and Re-Transform the Target Variable
#' @name mlr_graphs_targettrafo
#' @description
#' Wraps a [`Graph`] that transforms a target during training and inverts the transformation
#' during prediction. This is done as follows:
#' * Specify a transformation and inversion function using any subclass of [`PipeOpTargetTrafo`], defaults to
#'   [`PipeOpTargetTrafoSimple`], afterwards apply `graph`.
#' * At the very end, during prediction the transformation is inverted using [`PipeOpTargetInverter`].
#' * To set a transformation and inversion function for [`PipeOpTargetTrafoSimple`] see the
#'   parameters `trafo` and `inverter` of the `param_set` of the resulting [`Graph`].
#'
#' @param graph [`PipeOpLearner`] | [`Graph`] \cr
#'   A [`PipeOpLearner`] or [`Graph`] to create a robustifying pipeline for. If this is a [`Graph`],
#'   the last [`PipeOp`] should be a [`PipeOpLearner`] and the first [`PipeOp`] should accept a
#'   single [`Task`][mlr3::Task] as input.
#' @param trafo_pipeop [`PipeOp`] \cr
#'   A [`PipeOp`] that is a subclass of [`PipeOpTargetTrafo`]. Default is
#'   [`PipeOpTargetTrafoSimple`].
#' @param id_prefix `character(1)` \cr
#'   Optional id prefix to prepend to [`PipeOpTargetInverter`] ID. The resulting ID will be
#'   `"[id_prefix]targetinverter"`. Default is `""`.
#'
#' @return [`Graph`]
#' @export
#' @examples
#' library("mlr3")
#'
#' tt = pipeline_targettrafo(PipeOpLearner$new(LearnerRegrRpart$new()))
#' tt$param_set$values$targettrafosimple.trafo = function(x) log(x, base = 2)
#' tt$param_set$values$targettrafosimple.inverter = function(x) 2 ^ x
#'
#' # gives the same as
#' g = Graph$new()
#' g$add_pipeop(PipeOpTargetTrafoSimple$new(param_vals = list(
#'   trafo = function(x) log(x, base = 2),
#'   inverter = function(x) 2 ^ x)
#'   )
#' )
#' g$add_pipeop(LearnerRegrRpart$new())
#' g$add_pipeop(PipeOpTargetInverter$new())
#' g$add_edge(src_id = "targettrafosimple", dst_id = "targetinverter",
#'   src_channel = 1, dst_channel = 1)
#' g$add_edge(src_id = "targettrafosimple", dst_id = "regr.rpart",
#'   src_channel = 2, dst_channel = 1)
#' g$add_edge(src_id = "regr.rpart", dst_id = "targetinverter",
#'   src_channel = 1, dst_channel = 2)
pipeline_targettrafo = function(graph, trafo_pipeop = PipeOpTargetTrafoSimple$new(), id_prefix = "") {
  graph = as_graph(graph)
  assert_r6(graph$pipeops[length(graph$pipeops)][[1L]], classes = "PipeOpLearner")
  if (graph$pipeops[[graph$input$op.id]]$innum != 1L) {
    stopf("First PipeOp of graph should accept a single task as input.")
  }
  assert_r6(trafo_pipeop, classes = "PipeOpTargetTrafo")
  assert_string(id_prefix)

  ids = graph$ids(sorted = FALSE)
  target_inverter_id = paste0(id_prefix, "targetinverter")

  graph$add_pipeop(trafo_pipeop)
  graph$add_pipeop(PipeOpTargetInverter$new(target_inverter_id))

  graph$add_edge(src_id = trafo_pipeop$id, dst_id = target_inverter_id, src_channel = 1L, dst_channel = 1L)
  graph$add_edge(src_id = trafo_pipeop$id, dst_id = ids[1L], src_channel = 2L, dst_channel = 1L)
  graph$add_edge(src_id = ids[length(ids)], dst_id = target_inverter_id, src_channel = 1L, dst_channel = 2L)

  graph
}

mlr_graphs$add("targettrafo", pipeline_targettrafo)

#' @title Create Disjoint Graph Union of Copies of a Graph
#'
#' @description
#' Create a new [`Graph`] containing `n` copies of the input [`Graph`] / [`PipeOp`]. To avoid ID
#' collisions, PipeOp IDs are suffixed with `_i` where `i` ranges from 1 to `n`.
#'
#' @param graph [`Graph`] \cr
#'   Graph to replicate.
#' @param n `integer(1)`
#'   Number of copies to create.
#' @return [`Graph`] containing `n` copies of input `graph`.
#' @family Graph operators
#' @export
#' @examples
#' library("mlr3")
#'
#' po_pca = po("pca")
#' pipeline_greplicate(po_pca, n = 2)
pipeline_greplicate = function(graph, n) {
  graph = as_graph(graph)
  n = assert_count(n, positive = TRUE, coerce = TRUE)
  x = map(seq_len(n), function(i) {
    g = graph$clone(deep = TRUE)
    g$update_ids(postfix = paste0("_", i))
  })

  gunion(x)
}

mlr_graphs$add("greplicate", pipeline_greplicate)
