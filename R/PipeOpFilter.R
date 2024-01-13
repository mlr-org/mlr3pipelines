#' @title Feature Filtering
#'
#' @usage NULL
#' @name mlr_pipeops_filter
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Feature filtering using a [`mlr3filters::Filter`] object, see the
#' \CRANpkg{mlr3filters} package.
#'
#' If a `Filter` can only operate on a subset of columns based on column type, then only these features are considered and filtered.
#' `nfeat` and `frac` will count for the features of the type that the `Filter` can operate on;
#' this means e.g. that setting `nfeat` to 0 will only remove features of the type that the `Filter` can work with.
#'
#' @section Construction:
#' ```
#' PipeOpFilter$new(filter, id = filter$id, param_vals = list())
#' ```
#' * `filter` :: [`Filter`][mlr3filters::Filter]\cr
#'   [`Filter`][mlr3filters::Filter] used for feature filtering.
#'  This argument is always cloned; to access the [`Filter`][mlr3filters::Filter] inside `PipeOpFilter` by-reference, use `$filter`.\cr
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, defaulting to the `id` of the [`Filter`][mlr3filters::Filter] being used.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with features removed that were filtered out.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `scores` :: named `numeric`\cr
#'   Scores calculated for all features of the training [`Task`][mlr3::Task] which are being used
#'   as cutoff for feature filtering. If `frac` or `nfeat` is given, the underlying [`Filter`][mlr3filters::Filter] may choose to not calculate scores for
#'   all features that are given. This only includes features on which the [`Filter`][mlr3filters::Filter] can operate; e.g.
#'   if the [`Filter`][mlr3filters::Filter] can only operate on numeric features, then scores for factorial features will not be given.
#' * `features` :: `character`\cr
#'   Names of features that are being kept. Features of types that the [`Filter`][mlr3filters::Filter] can not operate on are always being kept.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpTaskPreproc`], as well as the parameters of the [`Filter`][mlr3filters::Filter]
#' used by this object. Besides, parameters introduced are:
#' * `filter.nfeat` :: `numeric(1)` \cr
#'   Number of features to select.
#'   Mutually exclusive with `frac`, `cutoff`, and `permuted`.
#' * `filter.frac` :: `numeric(1)` \cr
#'   Fraction of features to keep.
#'   Mutually exclusive with `nfeat`, `cutoff`, and `permuted`.
#' * `filter.cutoff` :: `numeric(1)` \cr
#'   Minimum value of filter heuristic for which to keep features.
#'   Mutually exclusive with `nfeat`, `frac`, and `permuted`.
#' * `filter.permuted` :: `integer(1)` \cr
#'   If this parameter is set, a random permutation of each feature is added to the task before
#'   applying the filter. All features selected before the `permuted`-th permuted features is selected
#'   are kept. This is similar to the approach in `r cite_bib("wu2007", "thomas2017")`.
#'   Mutually exclusive with `nfeat`, `frac`, and `cutoff`.
#'
#' Note that at least one of `filter.nfeat`, `filter.frac`, `filter.cutoff`, and `filter.permuted` must be given.
#'
#' @section Internals:
#' This does *not* use the `$.select_cols` feature of [`PipeOpTaskPreproc`] to select only features compatible with the [`Filter`][mlr3filters::Filter];
#' instead the whole [`Task`][mlr3::Task] is used by `private$.get_state()` and subset internally.
#'
#' @section Fields:
#' Fields inherited from [`PipeOpTaskPreproc`], as well as:
#' * `filter` :: [`Filter`][mlr3filters::Filter]\cr
#'   [`Filter`][mlr3filters::Filter] that is being used for feature filtering. Do *not* use this slot to get to the feature filtering scores
#'   after training; instead, use `$state$scores`. Read-only.
#'
#' @section Methods:
#' Methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @references
#' `r format_bib("wu2007", "thomas2017")`
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#' library("mlr3filters")
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # setup PipeOpFilter to keep the 5 most important
#' # features of the spam task w.r.t. their AUC
#' task = tsk("spam")
#' filter = flt("auc")
#' po = po("filter", filter = filter)
#' po$param_set
#' po$param_set$values$filter.nfeat = 5
#'
#' # filter the task
#' filtered_task = po$train(list(task))[[1]]
#'
#' # filtered task + extracted AUC scores
#' filtered_task$feature_names
#' head(po$state$scores, 10)
#'
#' # feature selection embedded in a 3-fold cross validation
#' # keep 30% of features based on their AUC score
#' task = tsk("spam")
#' gr = po("filter", filter = flt("auc"), filter.frac = 0.5) %>>%
#'   po("learner", lrn("classif.rpart"))
#' learner = GraphLearner$new(gr)
#' rr = resample(task, learner, rsmp("holdout"), store_models = TRUE)
#' rr$learners[[1]]$model$auc$scores
PipeOpFilter = R6Class("PipeOpFilter",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    filter = NULL,
    initialize = function(filter, id = filter$id, param_vals = list()) {
      assert_class(filter, "Filter")
      self$filter = filter$clone(deep = TRUE)
      if (paradox_info$is_old) {
        self$filter$param_set$set_id = ""
        map(self$filter$param_set$params, function(p) p$tags = union(p$tags, "train"))
      } else {
        for (pn in self$filter$param_set$ids()) {
          self$filter$param_set$tags[[pn]] = union(self$filter$param_set$tags[[pn]] , "train")
        }
      }
      private$.outer_param_set = ParamSet$new(params = list(
        nfeat = p_int(lower = 0, tags = "train"),
        frac = p_dbl(lower = 0, upper = 1, tags = "train"),
        cutoff = p_dbl(tags = "train"),
        permuted = p_int(lower = 1, tags = "train")
      ))
      if (paradox_info$is_old) {
        private$.outer_param_set$set_id = "filter"
      }
      super$initialize(id, alist(filter = private$.outer_param_set, self$filter$param_set), param_vals = param_vals, tags = "feature selection")
    }
  ),
  private = list(
    .outer_param_set = NULL,

    .get_state = function(task) {
      # reset filter on exit, the user should not even feel the temptation to not use the `$state`
      on.exit({self$filter$scores = structure(numeric(0), .Names = character(0))})
      filtercrit = c("nfeat", "frac", "cutoff", "permuted")
      filtercrit = Filter(function(name) !is.null(private$.outer_param_set$values[[name]]), filtercrit)
      if (length(filtercrit) != 1) {
        stopf("Exactly one of 'nfeat', 'frac', 'cutoff', or 'permuted' must be given. Instead given: %s",
          if (length(filtercrit) == 0) "none" else str_collapse(filtercrit))
      }
      critvalue = private$.outer_param_set$values[[filtercrit]]

      # note: we *could* use PipeOpTaskPreproc's 'select_cols' functionality here, however that would
      # lead to `$state$features` not mentioning the features taken out by `select_cols`.
      # Instead, we opt to clone the task and calculate filter values only on the reduced clone.
      filtertask = task$clone()
      filtertask$select(filtertask$feature_types[get("type") %in% self$filter$feature_types, get("id")])

      nfeat = switch(filtercrit,
        nfeat = critvalue,
        frac = round(length(filtertask$feature_names) * critvalue),
        permuted = length(task$feature_names) + critvalue,
        NULL)

      if (filtercrit == "permuted") {
        permuted = map_dtc(filtertask$data(cols = task$feature_names), shuffle)
        setnames(permuted, sprintf(".__permuted__%s", names(permuted)))
        filtertask$cbind(permuted)
      }

      self$filter$calculate(filtertask, nfeat)
      scores = self$filter$scores

      features = switch(filtercrit,
        cutoff = names(scores)[scores >= critvalue],
        nfeat = utils::head(names(scores), nfeat),
        frac = utils::head(names(scores), nfeat),
        permuted = {
          idx = wf(cumsum(startsWith(names(scores), ".__permuted__")) == critvalue)
          utils::head(names(scores), if (length(idx)) idx - 1L else Inf)
        },
        stop("unknown filter criterion"))

      # the features only relate to the features in `filtertask`, we want a vector of *all* features to keep
      features = setdiff(task$feature_names, setdiff(filtertask$feature_names, features))

      # we don't use 'scores', but maybe the user cares.
      # In particular, the user can *not* rely on the self$filter object being set, because
      # `$state` is the only place that the user may rely on being changed after `$train()`.
      list(scores = scores, features = features)
    },

    .transform = function(task) {
      task$select(self$state$features)
    },
    .additional_phash_input = function() class(self$filter)
  )
)

mlr_pipeops$add("filter", PipeOpFilter, list(R6Class("Filter", public = list(id = "dummyfilter", param_set = ParamSet$new()))$new()))
