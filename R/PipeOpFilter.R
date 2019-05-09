#' @title PipeOpFilter
#'
#' @name mlr_pipeop_filter
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`].
#'
#' @description
#' Feature filtering using a [`mlr3featsel::Filter`] object, see the
#' [mlr3featsel][mlr3::mlr3-package] package.
#'
#' The `settings` of the filter are given as [`paradox::ParamUty`];
#' when `Filter`s start supporting the `paradox` interface that could
#' be used instead.
#'
#' If a `Filter` can only operate on a subset of columns based on column
#' type, then only these features are considered. `nfeat` and `frac` will
#' count for the features of the type that the `Filter` can operate on;
#' this means e.g. that setting `nfeat` to 0 will only remove features of the
#' type that the `Filter` can work with.
#'
#' @section Methods:
#' * `PipeOpFilter$new(filter, id = filter$id, param_vals = list())` \cr
#'   ([`mlr3featsel::Filter`], `character(1)`, `list`) -> `self` \cr
#'   Constructor. `filter` gives the `Filter` to use.
#' @section Parameter Set:
#' * `settings` :: named `list` \cr
#'   List of settings to be given to the `Filter`. Default `list()`.
#' * `nfeat`    :: `numeric(1)` \cr
#'   Number of features to select. If this is set, `frac` and cutoff`
#'   must not be set.
#' * `frac`     :: `numeric(1)` \cr
#'   Fraction of features to keep. If this is set, `nfeat` and `cutoff`
#'   must not be set.
#' * `cutoff`   :: `numeric(1)` \cr
#'   Minimum value of filter heuristic for which to keep features. If
#'   this is set, `nfeat` and `frac` must not be set.
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpFilter = R6Class("PipeOpFilter",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    filter = NULL,
    initialize = function(filter, id = filter$id, param_vals = list()) {
      assert_class(filter, "Filter")
      self$filter = filter$clone(deep = TRUE)
      self$filter$param_set$set_id = filter$id
      private$.outer_param_set = ParamSet$new(list(
        ParamInt$new("nfeat", lower = 0),
        ParamDbl$new("frac", lower = 0, upper = 1),
        ParamDbl$new("cutoff")
      ))
      private$.outer_param_set$set_id = "filter"
      super$initialize(id, self$param_set, param_vals = param_vals)
    },

    get_state = function(task) {

      filtercrit = c("nfeat", "frac", "cutoff")
      filtercrit = Filter(function(name) !is.null(private$.outer_param_set$values[[name]]), filtercrit)
      if (length(filtercrit) != 1) {
        stopf("Exactly one of 'nfeat', 'frac', 'cutoff' must be given. Instead given: %s",
          if (length(filtercrit) == 0) "none" else str_collapse(filtercrit))
      }
      critvalue = private$.outer_param_set$values[[filtercrit]]

      filtertask = task$clone()
      filtertask$select(filtertask$feature_types[get("type") %in% self$filter$feature_types, get("id")])
      maxfeat = length(filtertask$feature_names)

      self$filter$calculate(filtertask)

      values = sort(self$filter$scores, decreasing = TRUE)
      features = switch(filtercrit,
        cutoff = names(values)[values >= critvalue],
        nfeat = names(values)[seq_len(min(maxfeat, critvalue))],
        frac = names(values)[seq_len(round(maxfeat * critvalue))],
        stop("unknown filter criterion"))
      # the features only relate to the features in `filtertask`, we want a vector of *all* features to keep
      features = setdiff(task$feature_names, setdiff(filtertask$feature_names, features))

      list(values = values, features = features) # we don't use 'values', but maybe the user cares.
    },

    transform = function(task) {
      task$select(self$state$features)
    }),
  active = list(
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        private$.param_set = ParamSetCollection$new(list(
          private$.outer_param_set,
          self$filter$param_set
        ))
        private$.param_set$set_id = self$id %??% self$filter$id # self$id may be NULL during initialize() call
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }),
  private = list(
    deep_clone = function(name, value) {
      private$.param_set = NULL # required to keep clone identical to original, otherwise tests get really ugly
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    },
    .outer_param_set = NULL
  )
)
