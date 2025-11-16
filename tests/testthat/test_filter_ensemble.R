context("FilterEnsemble")

test_that("FilterEnsemble combines wrapped filter scores", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("sonar")
  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  flt_ensemble = FilterEnsemble$new(lapply(filters, function(flt) flt$clone(deep = TRUE)))

  weights = c(0.3, 0.7)
  names(weights) = vapply(filters, function(flt) flt$id, character(1))
  flt_ensemble$param_set$values$weights = weights

  flt_ensemble$calculate(task)
  combined_scores = flt_ensemble$scores

  individual_scores = lapply(filters, function(flt) {
    flt$calculate(task)
    flt$scores[task$feature_names]
  })

  expected = Reduce(`+`, Map(function(scores, w) scores * w, individual_scores, weights))
  expect_equal(combined_scores[task$feature_names], expected)

  flt_ensemble$param_set$values$rank_transform = TRUE
  flt_ensemble$calculate(task)
  combined_rank_scores = flt_ensemble$scores

  expected_rank = Reduce(`+`, Map(function(scores, w) rank(scores) * w, individual_scores, weights))
  expect_equal(combined_rank_scores[task$feature_names], expected_rank)
})

test_that("FilterEnsemble with a single filter is a passthrough", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("sonar")
  variance_ref = mlr3filters::FilterVariance$new()
  variance_ref$calculate(task)

  ensemble = FilterEnsemble$new(list(mlr3filters::FilterVariance$new()))
  ensemble$param_set$values$weights = c(variance = 1)
  ensemble$calculate(task)

  expect_equal(ensemble$scores[task$feature_names], variance_ref$scores[task$feature_names])
})

test_that("FilterEnsemble with one-hot weights selects the active filter", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("sonar")
  variance_filter = mlr3filters::FilterVariance$new()
  auc_filter = mlr3filters::FilterAUC$new()

  variance_filter$calculate(task)
  auc_filter$calculate(task)

  weights = c(variance = 1, auc = 0)
  ensemble = FilterEnsemble$new(list(
    variance_filter$clone(deep = TRUE),
    auc_filter$clone(deep = TRUE)
  ))
  ensemble$param_set$values$weights = weights
  ensemble$calculate(task)

  expect_equal(ensemble$scores[task$feature_names], variance_filter$scores[task$feature_names])
})

test_that("FilterEnsemble is registered in mlr_filters", {
  skip_if_not_installed("mlr3filters")
  expect_true("ensemble" %in% mlr3filters::mlr_filters$keys())
})

test_that("FilterEnsemble identifier concatenates wrapped filter ids", {
  skip_if_not_installed("mlr3filters")

  ensemble = FilterEnsemble$new(list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  ))

  expect_identical(ensemble$id, "variance.auc")
})

test_that("FilterEnsemble reorders named weights correctly", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("sonar")
  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  flt_ensemble = FilterEnsemble$new(filters)

  var_filter = mlr3filters::FilterVariance$new()
  auc_filter = mlr3filters::FilterAUC$new()
  var_filter$calculate(task)
  auc_filter$calculate(task)
  variance_scores = var_filter$scores[task$feature_names]
  auc_scores = auc_filter$scores[task$feature_names]

  weights = c(auc = 0.8, variance = 0.2)
  flt_ensemble$param_set$values$weights = weights
  flt_ensemble$calculate(task)

  expected = weights[["variance"]] * variance_scores + weights[["auc"]] * auc_scores
  expect_equal(flt_ensemble$scores[task$feature_names], expected)
})

test_that("FilterEnsemble respects unnamed weight order", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("sonar")
  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  flt_ensemble = FilterEnsemble$new(filters)

  var_filter = mlr3filters::FilterVariance$new()
  auc_filter = mlr3filters::FilterAUC$new()
  var_filter$calculate(task)
  auc_filter$calculate(task)
  variance_scores = var_filter$scores[task$feature_names]
  auc_scores = auc_filter$scores[task$feature_names]

  weights = c(0.7, 0.3)
  flt_ensemble$param_set$values$weights = weights
  flt_ensemble$calculate(task)
  expected = weights[1] * variance_scores + weights[2] * auc_scores
  expect_equal(flt_ensemble$scores[task$feature_names], expected)
})

test_that("FilterEnsemble weight input validation", {
  skip_if_not_installed("mlr3filters")

  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  flt_ensemble = FilterEnsemble$new(filters)

  expect_error(
    flt_ensemble$param_set$set_values(weights = c(foo = 0.5, bar = 0.5)),
    "permutation"
  )
  expect_error(
    flt_ensemble$param_set$set_values(weights = c(0.5)),
    "length"
  )
  expect_error(
    flt_ensemble$param_set$set_values(weights = c(0, 0)),
    "At least one weight"
  )
})

test_that("FilterEnsemble requires at least one wrapped filter", {
  skip_if_not_installed("mlr3filters")
  expect_error(FilterEnsemble$new(list()), "length >= 1")
})

test_that("FilterEnsemble task types are intersected", {
  skip_if_not_installed("mlr3filters")

  ensemble = FilterEnsemble$new(list(
    mlr3filters::FilterAnova$new(),             # classif
    mlr3filters::FilterCorrelation$new(),       # regr
    mlr3filters::FilterVariance$new()           # any
  ))

  expect_identical(ensemble$task_types, character())
})

test_that("FilterEnsemble feature types intersect across filters", {
  skip_if_not_installed("mlr3filters")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("rpart")

  importance_filter = mlr3filters::FilterImportance$new(mlr3::lrn("classif.rpart"))
  variance_filter = mlr3filters::FilterVariance$new()

  ensemble = FilterEnsemble$new(list(
    variance_filter,
    importance_filter
  ))

  expect_setequal(ensemble$feature_types, c("integer", "numeric"))
})

test_that("FilterEnsemble aggregates packages and task properties", {
  skip_if_not_installed("mlr3filters")

  ensemble = FilterEnsemble$new(list(
    mlr3filters::FilterVariance$new(), # stats
    mlr3filters::FilterAUC$new()       # mlr3measures, twoclass property
  ))

  expect_true(all(c("stats", "mlr3measures") %in% ensemble$packages))
  expect_identical(ensemble$task_properties, "twoclass")
})

test_that("FilterEnsemble clones wrapped filters on construction", {
  skip_if_not_installed("mlr3filters")

  original_variance = mlr3filters::FilterVariance$new()
  ensemble = FilterEnsemble$new(list(original_variance))

  variance_in_ensemble = ensemble$wrapped$variance
  variance_in_ensemble$param_set$values$na.rm = FALSE
  expect_true(original_variance$param_set$values$na.rm)
})

test_that("FilterEnsemble works inside PipeOpFilter", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("sonar")
  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  flt_ensemble = FilterEnsemble$new(lapply(filters, function(flt) flt$clone(deep = TRUE)))

  weights = c(variance = 0.5, auc = 0.5)
  flt_ensemble$param_set$values$weights = weights
  flt_ensemble$calculate(task)

  ordered_features = names(sort(flt_ensemble$scores, decreasing = TRUE))

  po = PipeOpFilter$new(flt_ensemble$clone(deep = TRUE),
    param_vals = list(filter.nfeat = 5, weights = weights)
  )

  filtered_task = po$train(list(task))[[1]]
  expect_setequal(filtered_task$feature_names, ordered_features[seq_len(5)])
})

test_that("FilterEnsemble parameters are accessible through PipeOpFilter", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("sonar")
  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  ensemble = FilterEnsemble$new(filters)

  po = PipeOpFilter$new(ensemble$clone(deep = TRUE))
  po$param_set$set_values(
    filter.nfeat = 5,
    weights = c(variance = 0.5, auc = 0.5),
    variance.na.rm = FALSE
  )

  po$train(list(task))
  expect_false(po$filter$wrapped$variance$param_set$get_values()[["na.rm"]])
})

test_that("FilterEnsemble cloning keeps param sets independent", {
  skip_if_not_installed("mlr3filters")

  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )

  original = FilterEnsemble$new(filters)
  original$param_set$values$weights = stats::setNames(c(0.4, 0.6), vapply(filters, function(flt) flt$id, character(1)))

  clone = original$clone(deep = TRUE)

  clone$param_set$values[[paste0(filters[[1]]$id, ".na.rm")]] = FALSE

  expect_false(clone$wrapped[[filters[[1]]$id]]$param_set$get_values()[["na.rm"]])
  expect_true(original$wrapped[[filters[[1]]$id]]$param_set$get_values()[["na.rm"]])
})

test_that("FilterEnsemble ignores NA scores from wrapped filters", {
  skip_if_not_installed("mlr3filters")

  task = mlr_tasks$get("iris")
  variance_filter = mlr3filters::FilterVariance$new()
  permutation_filter = mlr3filters::FilterPermutation$new()

  permutation_filter$resampling$instantiate(task)
  permutation_filter$param_set$values$standardize = TRUE
  permutation_filter$param_set$values$nmc = 1L

  variance_filter$calculate(task)
  permutation_filter$calculate(task)

  variance_scores = variance_filter$scores[task$feature_names]
  expect_true(all(is.nan(permutation_filter$scores[task$feature_names])))

  filters = list(
    variance_filter$clone(deep = TRUE),
    permutation_filter$clone(deep = TRUE)
  )
  weights = c(variance = 0.5, permutation = 0.5)

  flt_ensemble = FilterEnsemble$new(filters)
  flt_ensemble$param_set$values$weights = weights
  flt_ensemble$calculate(task)

  combined_scores = flt_ensemble$scores[task$feature_names]
  expect_equal(combined_scores, variance_scores * weights[["variance"]])
})

test_that("FilterEnsemble rank transform ignores NA scores", {
  skip_if_not_installed("mlr3filters")
  skip_if_not_installed("paradox")

  DummyNAFilter = R6::R6Class("DummyNAFilter",
    inherit = mlr3filters::Filter,
    public = list(
      initialize = function() {
        super$initialize(
          id = "dummy_na",
          task_types = "classif",
          feature_types = c("integer", "numeric"),
          task_properties = character(),
          param_set = paradox::ps()
        )
      }
    ),
    private = list(
      .calculate = function(task, nfeat) {
        stats::setNames(rep(NA_real_, length(task$feature_names)), task$feature_names)
      }
    )
  )

  task = mlr_tasks$get("sonar")
  variance_filter = mlr3filters::FilterVariance$new()
  variance_filter$calculate(task)
  variance_scores = variance_filter$scores[task$feature_names]

  filters = list(
    variance_filter$clone(deep = TRUE),
    DummyNAFilter$new()
  )
  weights = c(variance = 0.6, dummy_na = 0.4)

  flt_ensemble = FilterEnsemble$new(filters)
  flt_ensemble$param_set$values$weights = weights
  flt_ensemble$param_set$values$rank_transform = TRUE
  flt_ensemble$calculate(task)

  combined_scores = flt_ensemble$scores[task$feature_names]
  expected_rank = rank(variance_scores, na.last = "keep", ties.method = "average")
  expect_equal(combined_scores, expected_rank * weights[["variance"]])
})

test_that("FilterEnsemble weight helper normalization works", {
  skip_if_not_installed("mlr3filters")
  skip_if_not_installed("paradox")

  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  ensemble = FilterEnsemble$new(filters)

  ps_no = ensemble$get_weights_search_space(normalize_weights = "no", prefix = "")
  vals_no = ps_no$trafo(list(
    variance = 0.2,
    auc = 0.8
  ))
  expect_equal(vals_no$weights, c(variance = 0.2, auc = 0.8))

  ps_naive = ensemble$get_weights_search_space(normalize_weights = "naive")
  vals_naive = ps_naive$trafo(list(
    w.variance = 2,
    w.auc = 6
  ))
  expect_equal(sum(vals_naive$weights), 1)

  ps_uniform = ensemble$get_weights_search_space(normalize_weights = "uniform")
  vals_uniform = ps_uniform$trafo(list(
    w.variance = 0.99,
    w.auc = 0.01
  ))
  expect_equal(sum(vals_uniform$weights), 1)
  expect_true(all(vals_uniform$weights > 0))

  ps_custom = ensemble$get_weights_search_space(
    weights_param_name = "custom.weights",
    prefix = "custom",
    normalize_weights = "no"
  )
  vals_custom = ps_custom$trafo(list(
    custom.variance = 0.4,
    custom.auc = 0.6
  ))
  expect_named(vals_custom$`custom.weights`, c("variance", "auc"))
})

test_that("FilterEnsemble weight helper tune tokens", {
  skip_if_not_installed("mlr3filters")

  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  ensemble = FilterEnsemble$new(filters)

  token = ensemble$get_weights_tunetoken(normalize_weights = "naive")
  expect_s3_class(token, "TuneToken")

  ensemble$set_weights_to_tune(normalize_weights = "naive")
  expect_s3_class(ensemble$param_set$values$weights, "TuneToken")
})

test_that("FilterEnsemble weight search space works with bbotk", {
  skip_if_not_installed("mlr3filters")
  skip_if_not_installed("bbotk")
  skip_if_not_installed("rpart")

  set.seed(1)
  task = mlr_tasks$get("sonar")

  filters = list(
    mlr3filters::FilterVariance$new(),
    mlr3filters::FilterAUC$new()
  )
  ensemble = FilterEnsemble$new(filters)
  po_filter = PipeOpFilter$new(ensemble$clone(deep = TRUE), id = "filter")
  po_filter$param_set$values$filter.frac = 0.5

  graph = po_filter %>>% po("learner", lrn("classif.rpart"))
  learner = GraphLearner$new(graph)
  measure = msr("classif.acc")
  resampling = rsmp("holdout")
  resampling$instantiate(task)

  ps_domain = ensemble$get_weights_search_space(normalize_weights = "uniform")

  obj = bbotk::ObjectiveRFun$new(
    fun = function(xs) {
      weights = xs$weights
      if (is.null(weights)) {
        xs = ps_domain$trafo(xs)
        weights = xs$weights
      }
      learner$param_set$values$filter.weights = weights
      rr = resample(task, learner, resampling)
      list(classif.acc = rr$score(measure)$classif.acc)
    },
    domain = ps_domain,
    codomain = paradox::ps(
      classif.acc = paradox::p_dbl(tags = "maximize", lower = 0, upper = 1)
    ),
    check_values = FALSE
  )

  terminator = bbotk::trm("evals", n_evals = 2)
  instance = bbotk::OptimInstanceSingleCrit$new(
    objective = obj,
    terminator = terminator
  )
  optimizer = bbotk::opt("random_search", batch_size = 1)
  optimizer$optimize(instance)

  expect_true(nrow(instance$archive$data) >= 2)
  expect_true(all(instance$archive$data$classif.acc <= 1))
})
