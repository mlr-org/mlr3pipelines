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
#'   A task to create a robustifying pipeline for.
#'   Optional, if omitted, the full pipeline is created.
#' @param learner [`Learner`] \cr
#'   A learner to create a robustifying pipeline for. Optional, if omitted,
#'   a more conservative pipeline is built.
#' @param impute_missings [`logical`] | [`NULL`] \cr
#'   Should missing values be imputed? Defaults to `NULL`, i.e imputes if the task has
#'   missing values and the learner can not handle them.
#' @param factors_to_numeric [`logical`] | [`NULL`] \cr
#'   Should factors be encoded? Defaults to `NULL`, i.e encodes if the task has factors
#'   and the learner can not handle factors.
#' @param max_cardinality [`integer`] \cr
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

  # FIXME: Uncomment once #308 is merged
  # if (has_type_feats("POSIXct") && "POSIXct" %nin% learner$feature_types))
  #   pos = c(pos, po("datefeatures", param_vals = list(affect_columns = selector_type("POSIXct"))))

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
#'   A `PipeOpLearner` or `Graph` to create a robustifying pipeline for.
#'   Outputs from the replicated `graph`s are connected with the `averager`
#' @param iterations [`integer`] \cr
#'   Number of bagging iterations. Defaults to 10.
#' @param frac [`numeric`] \cr
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
  subs_repls = greplicate(subs, iterations)

  if (!is.null(averager)) subs_repls %>>% averager else subs_repls
}

mlr_graphs$add("bagging", pipeline_bagging)



#' @title Branch Between Alternative Paths
#'
#' @description
#' Create a multiplexed graph.
#'
#' @param graphs (`[list of Graph]`):\cr
#'   Multiple graphs, possibly named. They all must have exactly
#'   one output. If any of the arguments are named, then all must have
#'   unique names.
#' @param prefix_branchops (`[character(1)]`):\cr
#'   Optional id prefix to prepend to [`PipeOpBranch`] and [`PipeOpUnbranch`] id. Their
#'   resulting IDs will be `"[prefix_branchops]branch"` and `"[prefix_branchops]unbranch"`.
#'   Default is `""`.
#' @param prefix_paths (`[logical(1) | character(1)]`):\cr
#'   Whether to add prefixes to graph IDs when performing gunion. Can be helpful to
#'   avoid ID clashes in resulting graph. Default `FALSE`. If this is `TRUE`, the prefixes
#'   are taken from the names of the input arguments if present or `"poX"` where X counts up. If this is
#'   a `character(1)`, it is a prefix that is added to the `PipeOp` IDs *additionally*
#'   to the input argument list.
#'
#' @export
#' @examples
#' library("mlr3")
#'
#' po_pca = po("pca")
#' po_nop = po("nop")
#'
#' branches = branch(pca = po_pca, nothing = po_nop)
#' # gives the same as
#' branches = c("pca", "nothing")
#' po("branch", branches) %>>%
#'   gunion(list(po_pca, po_nop)) %>>%
#'   po("unbranch", branches)
#'
#' branch(pca = po_pca, nothing = po_nop,
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
