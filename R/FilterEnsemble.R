

#' @title Filter Ensemble
#'
#' @usage NULL
#' @name mlr_filters_ensemble
#' @format [`R6Class`][R6::R6Class] object inheriting from [`Filter`][mlr3filters::Filter].
#'
#' @description
#' Implements the filte rensemble proposed in `r cite_bib("binder_2020")`.

FilterEnsemble = R6Class("FilterEnsemble", inherit = mlr3filters::Filter,
  public = list(
    initialize = function(filters) {
      private$.wrapped = assert_list(filters, types = "Filter", min.len = 1)
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
          check_numeric(x, len = length(fnames)) %check&&%
            (check_names(names(x), type = "unnamed") %check||%
              check_names(names(x), type = "unique", permutation.of = fnames))
          }, fnames),
          tags = "required"
        ),
        rank_transform = p_lgl(init = FALSE, tags = "required")
      )

      super$initialize(
        id = paste(fnames, collapse = "."),
        task_types = task_types,
        task_properties = unique(unlist(map(private$.wrapped, "task_properties"))),
        param_set = ParamSetCollection$new(c(list(.own_param_set), map(private$.wrapped, "param_set"))),
        feature_types = Reduce(intersect, map(private$.wrapped, "feature_types")),
        packages = unique(unlist(map(private$.wrapped, "packages"))),
        label = "meta",
        man = "mlr3pipelines::mlr_filters_ensemble"
      )
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
        x[[weights_param_name]] = w
        x
      }, innames, fnames, normalize_weights, weights_param_name)

      do.call(paradox::ps, domains)
    }
  ),
  private = list(
    .wrapped = NULL,
    .own_param_set = NULL,
    .calculate = function(task, nfeat) {
      pv = private$.own_param_set$get_values()
      fn = task$feature_names
      nfeat = length(fn)  # need to rank all features in an ensemble
      weights = pv$weights
      wnames = names(private$.wrapped)
      if (!is.null(names(weights))) {
        weights = weights[wnames]
      }
      scores = pmap(list(private$.wrapped, weights), function(x, w) {
        x$calculate(task, nfeat)
        s = x$scores[fn]
        if (pv$rank_transform) s = rank(s)
        s * w
      })
      structure(rowSums(as.data.frame(scores)), names = fn)
    }
  )

)
