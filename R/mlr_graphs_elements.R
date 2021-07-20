#' @include mlr_graphs.R

#' @title Robustify a learner
#' @name mlr_graphs_robustify
#' @description
#' Creates a [`Graph`] that can be used to robustify any subsequent learner.
#' Performs the following steps:
#' * Drops empty factor levels using [`PipeOpFixFactors`]
#' * Imputes `numeric` features using [`PipeOpImputeHist`] and [`PipeOpMissInd`]
#' * Imputes `factor` features using [`PipeOpImputeOOR`]
#' * Encodes `factors` using `one-hot-encoding`. Factors with a cardinality > max_cardinality are
#'   collapsed using [`PipeOpCollapseFactors`]
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
#'   Should missing values be imputed? Defaults to `NULL`, i.e. imputes if the task has
#'   missing values and the learner can not handle them.
#' @param factors_to_numeric `logical(1)` | `NULL` \cr
#'   Should factors be encoded? Defaults to `NULL`, i.e. encodes if the task has factors
#'   and the learner can not handle factors.
#' @param max_cardinality `integer(1)` \cr
#'   Maximum number of factor levels allowed. See above. Default: 1000.
#' @return [`Graph`]
#' @export
#' @examples
#' \donttest{
#' library(mlr3)
#' lrn = lrn("regr.rpart")
#' task = mlr_tasks$get("boston_housing")
#' gr = pipeline_robustify(task, lrn) %>>% po("learner", lrn)
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
#' }
pipeline_robustify = function(task = NULL, learner = NULL, impute_missings = NULL,
  factors_to_numeric = NULL, max_cardinality = 1000) {

  has_type_feats = function(types, if_null = TRUE) {
    if (is.null(task)) if_null else any(types %in% task$feature_types$type)
  }
  if (!is.null(task)) assert_task(task)
  if (!is.null(learner)) assert_learner(learner)
  if (is.null(impute_missings)) impute_missings = is.null(task) || (any(task$missings()) && (is.null(learner) || "missings" %nin% learner$properties))
  assert_flag(impute_missings)
  if (is.null(factors_to_numeric)) factors_to_numeric = is.null(task) || (has_type_feats("factor") && (is.null(learner) || "factor" %nin% learner$feature_types))
  assert_flag(impute_missings)
  assert_count(max_cardinality)

  # If given a task, only treat actually existing column types
  pos = list()

  # FIXME: Improve this when text-processors are available. See #332 and friends
  if (has_type_feats("character") && "character" %nin% learner$feature_types) {
    pos = c(pos, po("colapply", id = "char_to_fct", param_vals = list(affect_columns = selector_type("character"), applicator = function(x) as.factor(x))))
  }

  # Date processing
   if (has_type_feats("POSIXct") && ("POSIXct" %nin% learner$feature_types)) {
     pos = c(pos, po("datefeatures", param_vals = list(affect_columns = selector_type("POSIXct"))))
   }

  if (impute_missings) {
    # Impute numerics
    if (has_type_feats(c("numeric", "integer"))) {
      type = if (is.null(learner)) {
        "factor"  # default to factor as in PipeOpMissInd anyways
      } else {
        types = c("factor", "integer", "logical", "numeric")
        type_candidates = intersect(types, learner$feature_types)
        if (length(type_candidates) == 0L) {
          stopf("Learner %s does not support any of the feature types needed for missing indicator columns: %s.",
            learner$id, paste0(types, collapse = ", "))
        } else {
          type_candidates[[1]]  # just take the first supported type
        }
      }
      pos = c(pos,
        gunion(list(
          po("imputehist"),
          po("missind", param_vals = list(affect_columns = selector_type(c("numeric", "integer")), type = type)))) %>>%
        po("featureunion"))
    }
    # Impute factors
    if (has_type_feats(c("factor", "ordered", "character"))) {
      pos = c(pos, po("imputeoor"))
    }
  }

  # Fix extra factor levels
  if (has_type_feats(c("factor", "ordered"))) {
    pos = c(pos, po("fixfactors"))
  }

  # Ensure all factor levels are encoded during predict
  if (impute_missings && has_type_feats(c("factor", "ordered", "character"))) {
    pos = c(pos, po("imputesample", affect_columns = selector_type(c("factor", "ordered", "character"))))
  }

  # Collapse factors over 1000 levels
  # FIXME: Can be improved after #330 is solved
  if (is.null(task)) {
    pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
  } else {
    if (any(map_lgl(task$levels(task$feature_types$id[task$feature_types$type == "factor"]), function(x) length(x) > max_cardinality))) {
      pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
    }
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
#' * `Subsample` the data in each step using [`PipeOpSubsample`], afterwards apply `graph`
#' * Replicate this step `iterations` times (in parallel via [multiplicities][Multiplicity])
#' * Average outputs of replicated `graph`s predictions using the `averager`
#'   (note that setting `collect_multipliciy = TRUE` is required)
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
#'   predictions respectively.
#'   If `NULL` (default), no averager is added to the end of the graph.
#'   Note that setting `collect_multipliciy = TRUE` during construction of the averager is required.
#' @return [`Graph`]
#' @export
#' @examples
#' \donttest{
#' library(mlr3)
#' lrn_po = po("learner", lrn("regr.rpart"))
#' task = mlr_tasks$get("boston_housing")
#' gr = pipeline_bagging(lrn_po, 3, averager = po("regravg", collect_multiplicity = TRUE))
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
#' }
pipeline_bagging = function(graph, iterations = 10, frac = 0.7, averager = NULL) {
  g = as_graph(graph, clone = TRUE)
  assert_count(iterations)
  assert_number(frac, lower = 0, upper = 1)
  if (!is.null(averager)) {
    if (NROW(averager$input) != 1L || !(grepl("\\[*\\]", x = averager$input$train) && grepl("\\[*\\]", x = averager$input$predict))) {
      stop("'averager' must collect multiplicities.")
    }
    averager = as_graph(averager, clone = TRUE)
  }

  po("replicate", param_vals = list(reps = iterations)) %>>%
    po("subsample", param_vals = list(frac = frac)) %>>%
    g %>>%
    averager
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
#'   [`PipeOpTargetMutate`], afterwards apply `graph`.
#' * At the very end, during prediction the transformation is inverted using [`PipeOpTargetInvert`].
#' * To set a transformation and inversion function for [`PipeOpTargetMutate`] see the
#'   parameters `trafo` and `inverter` of the `param_set` of the resulting [`Graph`].
#' * Note that the input `graph` is not explicitly checked to actually return a
#'   [`Prediction`][mlr3::Prediction] during prediction.
#'
#' @param graph [`PipeOpLearner`] | [`Graph`] \cr
#'   A [`PipeOpLearner`] or [`Graph`] to wrap between a transformation and re-transformation of the target variable.
#' @param trafo_pipeop [`PipeOp`] \cr
#'   A [`PipeOp`] that is a subclass of [`PipeOpTargetTrafo`]. Default is [`PipeOpTargetMutate`].
#' @param id_prefix `character(1)` \cr
#'   Optional id prefix to prepend to [`PipeOpTargetInvert`] ID. The resulting ID will be `"[id_prefix]targetinvert"`. Default is `""`.
#'
#' @return [`Graph`]
#' @export
#' @examples
#' library("mlr3")
#'
#' tt = pipeline_targettrafo(PipeOpLearner$new(LearnerRegrRpart$new()))
#' tt$param_set$values$targetmutate.trafo = function(x) log(x, base = 2)
#' tt$param_set$values$targetmutate.inverter = function(x) list(response = 2 ^ x$response)
#'
#' # gives the same as
#' g = Graph$new()
#' g$add_pipeop(PipeOpTargetMutate$new(param_vals = list(
#'   trafo = function(x) log(x, base = 2),
#'   inverter = function(x) list(response = 2 ^ x$response))
#'   )
#' )
#' g$add_pipeop(LearnerRegrRpart$new())
#' g$add_pipeop(PipeOpTargetInvert$new())
#' g$add_edge(src_id = "targetmutate", dst_id = "targetinvert",
#'   src_channel = 1, dst_channel = 1)
#' g$add_edge(src_id = "targetmutate", dst_id = "regr.rpart",
#'   src_channel = 2, dst_channel = 1)
#' g$add_edge(src_id = "regr.rpart", dst_id = "targetinvert",
#'   src_channel = 1, dst_channel = 2)
pipeline_targettrafo = function(graph, trafo_pipeop = PipeOpTargetMutate$new(), id_prefix = "") {
  graph = as_graph(graph)
  if (graph$pipeops[[graph$input$op.id]]$innum != 1L) {
    stopf("First PipeOp of graph should accept a single task as input.")
  }
  assert_r6(trafo_pipeop, classes = "PipeOpTargetTrafo")
  assert_string(id_prefix)

  input_id = graph$input$op.id
  output_id = graph$output$op.id
  trafo_pipeop_id = trafo_pipeop$id
  target_invert_id = paste0(id_prefix, "targetinvert")

  graph$add_pipeop(trafo_pipeop)
  graph$add_pipeop(PipeOpTargetInvert$new(target_invert_id))

  graph$add_edge(src_id = trafo_pipeop_id, dst_id = target_invert_id, src_channel = 1L, dst_channel = 1L)
  graph$add_edge(src_id = trafo_pipeop_id, dst_id = input_id, src_channel = 2L, dst_channel = 1L)
  graph$add_edge(src_id = output_id, dst_id = target_invert_id, src_channel = 1L, dst_channel = 2L)

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



#' @title Create A Graph to Perform "One vs. Rest" classification.
#'
#' @description
#' Create a new [`Graph`] for a [classification Task][mlr3::TaskClassif] to
#' perform "One vs. Rest" classification.
#'
#' @param graph [`Graph`] \cr
#'   Graph being wrapped between [`PipeOpOVRSplit`] and [`PipeOpOVRUnite`].
#'   The Graph should return `NULL` during training and a
#'   [classification Prediction][mlr3::PredictionClassif] during prediction.
#' @return [`Graph`]
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("wine")
#'
#' learner = lrn("classif.rpart")
#' learner$predict_type = "prob"
#'
#' # Simple OVR
#' g1 = pipeline_ovr(learner)
#' g1$train(task)
#' g1$predict(task)
#'
#' # Bagged Learners
#' gr = po("replicate", reps = 3) %>>%
#'   po("subsample") %>>%
#'   learner %>>%
#'   po("classifavg", collect_multiplicity = TRUE)
#' g2 = pipeline_ovr(gr)
#' g2$train(task)
#' g2$predict(task)
#'
#' # Bagging outside OVR
#' g3 = po("replicate", reps = 3) %>>%
#'   pipeline_ovr(po("subsample") %>>% learner) %>>%
#'   po("classifavg", collect_multiplicity = TRUE)
#' g3$train(task)
#' g3$predict(task)
pipeline_ovr = function(graph) {
  graph = as_graph(graph)
  if (graph$output$train != "NULL" || graph$output$predict != "PredictionClassif") {
    stopf("Graph should return 'NULL' during training and a 'PredictionClassif' during prediction.")
  }
  g = graph$clone(deep = TRUE)
  PipeOpOVRSplit$new() %>>% g %>>% PipeOpOVRUnite$new()
}

mlr_graphs$add("ovr", pipeline_ovr)


#' @title Create A Graph to Perform Stacking.
#' 
#' @description 
#' Create a new [`Graph`] for stacking. A stacked learner uses predictions of
#' several base learners and fits a super learner using these predictions as
#' features in order to predict the outcome.
#' 
#' @param base_learners `list` of [`Learner`][mlr3::Learner]\cr
#'   A list of base learners.
#' @param super_learner [`Learner`][mlr3::Learner]\cr
#'   The super learner that makes the final prediction based on the base learners.
#' @param method `character(1)`\cr
#'   `"cv"` (default) for building a super learner using cross-validated predictions of the
#'   base learners or `"insample"` for building a super learner using the
#'   predictions of the base learners trained on all training data.
#' @param folds `ìnteger(1)`\cr
#'   Number of cross-validation folds. Only used for `method = "cv"`. Default 3.
#' @param use_features `logical(1)`\cr
#'   Whether the original features should also be passed to the super learner.
#'   Default `TRUE`.
#' @return [`Graph`]
#' 
#' @export 
#' @examples
#' if (requireNamespace("kknn")) {
#' library(mlr3)
#' library(mlr3learners)
#' 
#' base_learners = list(
#'   lrn("classif.rpart", predict_type = "prob"),
#'   lrn("classif.kknn", predict_type = "prob")
#' )
#' super_learner = lrn("classif.log_reg")
#' 
#' graph_stack = pipeline_stacking(base_learners, super_learner)
#' graph_learner = as_learner(graph_stack)
#' graph_learner$train(tsk("iris"))
#' }
pipeline_stacking = function(base_learners, super_learner, method = "cv", folds = 3, use_features = TRUE) {
  assert_learners(base_learners)
  assert_learner(super_learner)
  assert_choice(method, c("cv", "insample"))
  assert_flag(use_features)

  base_learners_cv = map(base_learners, po,
    .obj = "learner_cv", resampling.method = method, resampling.folds = folds
  )

  if (use_features) base_learners_cv = c(base_learners_cv, po("nop"))

  gunion(base_learners_cv) %>>%
     po("featureunion") %>>%
     super_learner
}

mlr_graphs$add("stacking", pipeline_stacking)
