#' @title PipeOpFilter
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
#'   Mutually exclusive with `frac` and `cutoff`.
#' * `filter.frac` :: `numeric(1)` \cr
#'   Fraction of features to keep.
#'   Mutually exclusive with `nfeat` and `cutoff`.
#' * `filter.cutoff` :: `numeric(1)` \cr
#'   Minimum value of filter heuristic for which to keep features.
#'   Mutually exclusive with `nfeat` and `frac`.
#'
#' Note that at least one of `filter.nfeat`, `filter.frac`, or `filter.cutoff` must be given.
#'
#' @section Internals:
#' This does *not* use the `$select_cols` feature of [`PipeOpTaskPreproc`] to select only features compatible with the [`Filter`][mlr3filters::Filter];
#' instead the whole [`Task`][mlr3::Task] is used by `$get_state()` and subset internally.
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
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#' library("mlr3filters")
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
      self$filter$param_set$set_id = ""
      map(self$filter$param_set$params, function(p) p$tags = union(p$tags, "train"))
      private$.outer_param_set = ParamSet$new(list(
        ParamInt$new("nfeat", lower = 0, tags = "train"),
        ParamDbl$new("frac", lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("cutoff", tags = "train")
      ))
      private$.outer_param_set$set_id = "filter"
      super$initialize(id, alist(private$.outer_param_set, self$filter$param_set), param_vals = param_vals, tags = "feature selection")
    },

    get_state = function(task) {
      # reset filter on exit, the user should not even feel the temptation to not use the `$state`
      on.exit({self$filter$scores = structure(numeric(0), .Names = character(0))})
      filtercrit = c("nfeat", "frac", "cutoff")
      filtercrit = Filter(function(name) !is.null(private$.outer_param_set$values[[name]]), filtercrit)
      if (length(filtercrit) != 1) {
        stopf("Exactly one of 'nfeat', 'frac', 'cutoff' must be given. Instead given: %s",
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
        NULL)

      self$filter$calculate(filtertask, nfeat)
      scores = self$filter$scores

      features = switch(filtercrit,
        cutoff = names(scores)[scores >= critvalue],
        nfeat = head(names(scores), nfeat),
        frac = head(names(scores), nfeat),
        stop("unknown filter criterion"))

      # the features only relate to the features in `filtertask`, we want a vector of *all* features to keep
      features = setdiff(task$feature_names, setdiff(filtertask$feature_names, features))

      # we don't use 'scores', but maybe the user cares.
      # In particular, the user can *not* rely on the self$filter object being set, because
      # `$state` is the only place that the user may rely on being changed after `$traion()`.
      list(scores = scores, features = features)
    },

    transform = function(task) {
      task$select(self$state$features)
    }
  ),
  private = list(
    .outer_param_set = NULL
  )
)

mlr_pipeops$add("filter", PipeOpFilter, list(R6Class("Filter", public = list(id = "dummyfilter", param_set = ParamSet$new()))$new()))
