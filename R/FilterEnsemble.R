#' @title Filter Ensemble
#'
#' @usage NULL
#' @name mlr_filters_ensemble
#' @format [`R6Class`][R6::R6Class] object inheriting from [`Filter`][mlr3filters::Filter].
#'
#' @description
#' `FilterEnsemble` aggregates several [`Filter`][mlr3filters::Filter]s by averaging their scores
#' (or ranks) with user-defined weights. Each wrapped filter is evaluated on the supplied task,
#' and the resulting feature scores are combined feature-wise by a convex combination determined
#' through the `weights` parameter. This allows leveraging complementary inductive biases of
#' multiple filters without committing to a single criterion. The concept was introduced by
#' Binder et al. (2020). This implementation follows the idea but leaves the exact choice of
#' weights to the user.
#'
#' @section Construction:
#' ```
#' FilterEnsemble$new(filters)
#' ```
#'
#' * `filters` :: `list` of [`Filter`][mlr3filters::Filter]\cr
#'   Filters that are evaluated and aggregated. Each filter must be cloneable and support the
#'   task type and feature types of the ensemble. The ensemble identifier defaults to the wrapped
#'   filter ids concatenated by `"."`.
#'
#' @section Parameters:
#' * `weights` :: `numeric()`\cr
#'   Required non-negative weights, one for each wrapped filter, with at least one strictly positive value.
#'   Values are used as given when calculating the weighted mean. If named, names must match the wrapped filter ids.
#' * `rank_transform` :: `logical(1)`\cr
#'   If `TRUE`, ranks of individual filter scores are used instead of the raw scores. Initialized to `FALSE`.
#' * `filter_score_transform` :: `function`\cr
#'   Function to be applied to the vector of individual filter scores after they were potentially transformed by 
#'   `rank_transform` but before weighting and aggregation. Initialized to `identity`.
#' * `result_score_transform` :: `function`\cr
#'   Function to be applied to the vector of aggregated scores after they were potentially transformed by `rank_transform` and/or 
#'   `filter_score_transform`. Initialized to `identity`.
#'
#' Parameters of wrapped filters are available via `$param_set` and can be referenced using
#' the wrapped filter id followed by `"."`, e.g. `"variance.na.rm"`.
#'
#' @section Fields:
#' * `$wrapped` :: named `list` of [`Filter`][mlr3filters::Filter]\cr
#'   Read-only access to the wrapped filters.
#'
#' @section Methods:
#' * `get_weights_search_space(weights_param_name = "weights", normalize_weights = "uniform", prefix = "w")`\cr
#'   (`character(1)`, `character(1)`, `character(1)`) -> [`ParamSet`][paradox::ParamSet]\cr
#'   Construct a [`ParamSet`][paradox::ParamSet] describing a weight search space.
#' * `get_weights_tunetoken(normalize_weights = "uniform")`\cr
#'   (`character(1)`) -> [`TuneToken`][paradox::TuneToken]\cr
#'   Shortcut returning a [`TuneToken`][paradox::TuneToken] for tuning the weights.
#' * `set_weights_to_tune(normalize_weights = "uniform")`\cr
#'   (`character(1)`) -> `self`\cr
#'   Convenience wrapper that stores the `TuneToken` returned by
#'   `get_weights_tunetoken()` in `$param_set$values$weights`.
#'
#' @section Internals:
#' All wrapped filters are called with `nfeat` equal to the number of features to ensure that
#' complete score vectors are available for aggregation. 
#' Scores are combined per feature by computing the weighted mean of (potentially transformed) scores or ranks.
#' Additionally, the final scores may also be transformed.
#' The order of transformations is as follows:
#' 1. `$calculate` the filter's scores for all features;
#' 2. If `rank_transform` is `TRUE`, convert filter scores to ranks;
#' 3. Apply `filter_score_transform` to the scores / ranks;
#' 4. Multiply the potentially transformed scores / ranks with the filter's weight
#' 5. Compute the element-wise sum across all weighted, transformed filter scores;
#' 6. Potentially apply `result_score_transform` to the vector of scores for each feature aggreagted across filters.
#' 
#' @section References:
#' `r format_bib("binder_2020")`
#'
#' @examplesIf mlr3misc::require_namespaces("mlr3filters", quietly = TRUE)
#' library("mlr3")
#' library("mlr3filters")
#'
#' task = tsk("sonar")
#'
#' filter = flt("ensemble",
#'   filters = list(FilterVariance$new(), FilterAUC$new()))
#' filter$param_set$values$weights = c(variance = 0.5, auc = 0.5)
#' filter$calculate(task)
#' head(as.data.table(flt))
#' 
#' # Aggregate reciprocal ranking
#' filter$param_set$set_values(rank_transform = TRUE, 
#'   filter_score_transform = function(x) 1 / x, 
#'   result_score_transform = function(x) rank(1 / x, ties.method = "average"))
#' filter$calculate(task)
#' head(as.data.table(filter))
#'
#' @export
FilterEnsemble = R6Class("FilterEnsemble", inherit = mlr3filters::Filter,
  public = list(
    initialize = function(filters) {
      private$.wrapped = lapply(assert_list(filters, types = "Filter", min.len = 1), function(x) x$clone(deep = TRUE))
      fnames = map_chr(private$.wrapped, "id")
      names(private$.wrapped) = fnames
      types_list = map(discard(private$.wrapped, function(x) test_scalar_na(x$task_types)), "task_types")
      if (length(types_list)) {
        task_types = Reduce(intersect, types_list)
      } else {
        task_types = NA_character_
      }
      .own_param_set = ps(
        weights = p_uty(custom_check = crate(function(x) {
          if (inherits(x, "TuneToken")) {
            return(TRUE)
          }
          check_numeric(x, len = length(fnames), lower = 0) %check&&%
            (check_names(names(x), type = "unnamed") %check||%
              check_names(names(x), type = "unique", permutation.of = fnames)) %check&&%
            (if (any(x > 0)) TRUE else "At least one weight must be > 0.")
          }, fnames),
          tags = "required"
        ),
        rank_transform = p_lgl(init = FALSE, tags = "required"),
        filter_score_transform = p_uty(init = identity, tags = "required", custom_check = check_function),
        result_score_transform = p_uty(init = identity, tags = "required", custom_check = check_function)
      )

      super$initialize(
        id = paste(fnames, collapse = "."),
        task_types = task_types,
        task_properties = unique(unlist(map(private$.wrapped, "task_properties"))),
        param_set = .own_param_set,
        feature_types = Reduce(intersect, map(private$.wrapped, "feature_types")),
        packages = unique(unlist(map(private$.wrapped, "packages"))),
        label = "meta",
        man = "mlr3pipelines::mlr_filters_ensemble"
      )
      private$.own_param_set = .own_param_set
      private$.param_set = NULL
    },
    get_weights_tunetoken = function(normalize_weights = "uniform") {
      assert_choice(normalize_weights, c("uniform", "naive", "no"))
      paradox::to_tune(self$get_weights_search_space(normalize_weights = normalize_weights))
    },
    set_weights_to_tune = function(normalize_weights = "uniform") {
      assert_choice(normalize_weights, c("uniform", "naive", "no"))
      self$param_set$set_values(.values = list(weights = self$get_weights_tunetoken(normalize_weights = normalize_weights)))
      invisible(self)
    },
    get_weights_search_space = function(weights_param_name = "weights", normalize_weights = "uniform", prefix = "w") {
      assert_string(prefix)
      assert_string(weights_param_name)
      assert_choice(normalize_weights, c("uniform", "naive", "no"))
      fnames = names(private$.wrapped)
      innames = if (prefix == "") fnames else paste0(prefix, ".", fnames)
      domains = rep(list(p_dbl(0, 1)), length(fnames))
      names(domains) = innames

      domains$.extra_trafo = crate(function(x) {
        w = unlist(x[innames], use.names = FALSE)
        names(w) = fnames
        x[innames] = NULL

        if (normalize_weights == "uniform") {
          w[w > 1 - .Machine$double.eps] = 1 - .Machine$double.eps
          w = -log1p(-w)
          w = w / max(sum(w), .Machine$double.eps)
        } else if (normalize_weights == "naive") {
          w = w / max(sum(w), .Machine$double.eps)
        }
        if (!any(w > 0)) {
          w[] = 1 / length(w)
        }
        x[[weights_param_name]] = w
        x
      }, innames, fnames, normalize_weights, weights_param_name)

      do.call(paradox::ps, domains)
    }
  ),
  private = list(
    .wrapped = NULL,
    .own_param_set = NULL,
    .param_set = NULL,
    .calculate = function(task, nfeat) {
      pv = private$.own_param_set$get_values()
      fn = task$feature_names
      nfeat = length(fn)  # need to rank all features in an ensemble
      weights = pv$weights
      wnames = names(private$.wrapped)

      if (!is.null(names(weights))) {
        weights = weights[wnames]
      }
      if (!any(weights > 0)) {
        stop("At least one weight must be > 0.")
      }

      # Calculate filter scores, weighted by weight for specific filter
      weighted_scores = pmap(list(private$.wrapped, weights), function(x, w) {
        x$calculate(task, nfeat)
        s = x$scores[fn]
        if (pv$rank_transform) s = rank(s, na.last = "keep", ties.method = "average")
        s = pv$filter_score_transform(s)
        # TODO: some sort of assert on result? e.g. length
        s * w
      })
      scores_dt = as.data.table(weighted_scores)

      # Aggregate across features
      combined = rowSums(scores_dt, na.rm = TRUE)
      # combined = apply(scores_dt, 1, function(row) weighted.mean(row, w = weights, na.rm = TRUE))  # weighted.mean normalizes weights in case of NAs
      combined = pv$result_score_transform(combined)
      # TODO: some sort of assert on result? e.g. length

      all_missing = rowSums(!is.na(scores_dt)) == 0L
      combined[all_missing] = NA_real_

      structure(combined, names = fn)
    },
    deep_clone = function(name, value) {
      if (name == ".wrapped") {
        private$.param_set = NULL
        return(map(value, function(x) x$clone(deep = TRUE)))
      }
      if (name == ".own_param_set") {
        private$.param_set = NULL
        return(value$clone(deep = TRUE))
      }
      if (name == ".param_set") {
        return(NULL)
      }
      value
    }
  ),
  active = list(
    wrapped = function(val) {
      if (!missing(val)) {
        stop("$wrapped is read-only.")
      }
      private$.wrapped
    },
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        private$.param_set = ParamSetCollection$new(c(list(private$.own_param_set), map(private$.wrapped, "param_set")))
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  )

)
