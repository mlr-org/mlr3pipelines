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
      ps = ParamSet$new(list(
        ParamUty$new("settings", default = list()),
        ParamInt$new("nfeat", lower = 0),
        ParamDbl$new("frac", lower = 0, upper = 1),
        ParamDbl$new("cutoff")
      ))
      super$initialize(id, ps, param_vals = param_vals)
    },

    get_state = function(task) {
      filtercrit = c("nfeat", "frac", "cutoff")
      filtercrit = Filter(function(name) !is.null(self$param_set$values[[name]]), filtercrit)
      if (length(filtercrit) != 1) {
        stopf("Exactly one of 'nfeat', 'frac', 'cutoff' must be given. Instead given: %s",
          if (length(filtercrit) == 0) "none" else str_collapse(filtercrit))
      }
      critvalue = self$param_set$values[[filtercrit]]

      settings = self$param_set$values$settings %??% list()

      filtertask = task$clone()
      filtertask$select(filtertask$feature_types[get("type") %in% self$filter$feature_types, get("id")])
      maxfeat = length(filtertask$feature_names)

      self$filter$calculate(filtertask, settings)

      values = sort(self$filter$filter_values, decreasing = TRUE)
      features = switch(filtercrit,
        cutoff = names(values)[values >= critvalue],
        nfeat = names(values)[seq_len(min(maxfeat, critvalue))],
        frac = names(values)[seq_len(round(maxfeat * critvalue))],
        stop("unknown filter criterion"))
      # the features only relate to the features in `filtertask`, we want a vector of *all* features to keep
      features = setdiff(task$feature_names, setdiff(filtertask$feature_names, features))

      list(values = values, features = features)  # we don't use 'values', but maybe the user cares.
    },

    transform = function(task) {
      task$select(self$state$features)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("filter", PipeOpFilter, list(R6Class("Filter", public = list(id = "filter"))$new()))

