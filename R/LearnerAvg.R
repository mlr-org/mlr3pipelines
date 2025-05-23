#' @title Optimized Weighted Average of Features for Classification and Regression
#'
#' @usage mlr_learners_classif.avg
#' @name mlr_learners_avg
#' @aliases mlr_learners_classif.avg
#' @format [`R6Class`][R6::R6Class] object inheriting from [`mlr3::LearnerClassif`]/[`mlr3::Learner`].
#'
#' @description
#' Computes a weighted average of inputs.
#' Used in the context of computing weighted averages of predictions.
#'
#' Predictions are averaged using `weights` (in order of appearance in the data) which are optimized using
#' nonlinear optimization from the package \CRANpkg{nloptr} for a measure provided in
#' `measure`. (defaults to `classif.ce` for `LearnerClassifAvg` and `regr.mse` for `LearnerRegrAvg`).
#' Learned weights can be obtained from `$model`.
#' This Learner implements and generalizes an approach proposed in `r cite_bib("ledell_2015")` that uses non-linear
#' optimization in order to learn base-learner weights that optimize a given performance metric (e.g `AUC`).
#' The approach is similar but not exactly the same as the one implemented as `AUC` in the \CRANpkg{SuperLearner}
#' R package (when `metric` is `"classif.auc"`).
#' For a more detailed analysis and the general idea, the reader is referred to `r cite_bib("ledell_2015")`.
#'
#' Note, that weights always sum to 1 by division by `sum(weights)` before weighting
#' incoming features.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`LearnerClassif`][mlr3::LearnerClassif], as well as:
#'  * `measure` :: [`Measure`][mlr3::Measure] | `character` \cr
#'    [`Measure`][mlr3::Measure] to optimize for.
#'    Will be converted to a [`Measure`][mlr3::Measure] in case it is `character`.
#'    Initialized to `"classif.ce"`, i.e. misclassification error for classification
#'    and `"regr.mse"`, i.e. mean squared error for regression.
#'  * `optimizer` :: [`Optimizer`][bbotk::Optimizer] | `character(1)`\cr
#'    [`Optimizer`][bbotk::Optimizer] used to find optimal thresholds.
#'    If `character`, converts to [`Optimizer`][bbotk::Optimizer]
#'    via [`opt`][bbotk::opt]. Initialized to `OptimizerNLoptr`.
#'    Nloptr hyperparameters are initialized to `xtol_rel = 1e-8`, `algorithm = "NLOPT_LN_COBYLA"`
#'    and equal initial weights for each learner.
#'    For more fine-grained control, it is recommended to supply a instantiated [`Optimizer`][bbotk::Optimizer].
#'  * `log_level` :: `character(1)` | `integer(1)`\cr
#'    Set a temporary log-level for `lgr::get_logger("mlr3/bbotk")`. Initialized to: "warn".
#'
#'
#' @section Methods:
#' * `LearnerClassifAvg$new(), id = "classif.avg")` \cr
#'   (`chr`) -> `self` \cr
#'   Constructor.
#' * `LearnerRegrAvg$new(), id = "regr.avg")` \cr
#'   (`chr`) -> `self` \cr
#'   Constructor.
#'
#' @references
#' `r format_bib("ledell_2015")`
#'
#' @family Learners
#' @family Ensembles
#' @include PipeOpEnsemble.R
#' @export
LearnerClassifAvg = R6Class("LearnerClassifAvg", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.avg") {
      ps = ps(
        measure = p_uty(custom_check = check_class_or_character("MeasureClassif", mlr_measures), tags = c("train", "required")),
        optimizer = p_uty(custom_check = check_optimizer, tags = c("train", "required")),
        log_level = p_uty(
          custom_check = crate(function(x) check_string(x) %check||% check_integerish(x)),
          tags = c("train", "required")
        )
      )
      ps$values = list(measure = "classif.ce", optimizer = "nloptr", log_level = "warn")
      super$initialize(
        id = id,
        param_set = ps,
        predict_types = c("response", "prob"),
        feature_types = c("integer", "numeric", "factor"),
        properties = c("twoclass", "multiclass"),
        man = "mlr3pipelines::LearnerClassifAvg"
      )
    },
    prepare_data = function(task) {
      data = task$data(cols = task$feature_names)
      fcts = map_lgl(data, is.factor)
      assert_true(all(fcts) || !any(fcts))  # TODO: nicer error message

      if (all(fcts) != (self$predict_type == "response")) {
        stopf("Trying to predict %s, but incoming data has %sfactors", self$predict_type, if (all(fcts)) "only " else "no ")
      }
      if (self$predict_type == "response") {
        alllevels = task$class_names
        map(data, function(x) assert_set_equal(alllevels, levels(x)))
        data
      } else {
        searchstring = paste0(".prob.", task$class_names[1])
        tg = task$feature_names
        tg = tg[which(substr(tg, nchar(tg) - nchar(searchstring) + 1, nchar(tg)) == searchstring)]
        inputstreams = substr(tg, 1, nchar(tg) - nchar(searchstring))
        assert_true(length(inputstreams) * length(task$class_names) == ncol(data))
        map(inputstreams, function(sn) {
          mat = as.matrix(data[, paste0(sn, ".prob.", task$class_names), with = FALSE])
          colnames(mat) = task$class_names
          mat
        })
      }
    },
    weighted_average_prediction = function(task, weights, data) {
      weights = weights / sum(weights)
      prob = NULL
      response = NULL
      if (self$predict_type == "response") {
        response = factor(task$class_names[max.col(weighted_factor_mean(data, weights, task$class_names))], levels = task$class_names)  # ties broken at random
      } else {
        prob = weighted_matrix_sum(data, weights)
        prob = pmin(pmax(prob, 0), 1)
      }

      PredictionClassif$new(row_ids = task$row_ids, truth = task$truth(), response = response, prob = prob)
    }
  ),
  private = list(
    .train = function(task) {
      data = self$prepare_data(task)
      n_weights = length(data)
      list("weights" = optimize_weights_learneravg(self, task, n_weights, data))
    },
    .predict = function(task) {
      self$weighted_average_prediction(task, self$model$weights, self$prepare_data(task))
    }
  )
)

#' @aliases mlr_learners_regr.avg
#' @usage mlr_learners_regr.avg
#' @rdname mlr_learners_avg
#' @export
LearnerRegrAvg = R6Class("LearnerRegrAvg", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.avg") {
      ps = ps(
        measure = p_uty(custom_check = check_class_or_character("MeasureRegr", mlr_measures), tags = c("train", "required")),
        optimizer = p_uty(custom_check = check_optimizer, tags = c("train", "required")),
        log_level = p_uty(tags = c("train", "required"),
          function(x) check_string(x) %check||% check_integerish(x))
      )
      ps$values = list(measure = "regr.mse", optimizer = "nloptr", log_level = "warn")
      super$initialize(
        id = id,
        param_set = ps,
        predict_types = "response",
        feature_types = c("integer", "numeric"),
        man = "mlr3pipelines::LearnerRegrAvg"
      )
    },
    prepare_data = function(task) {
      task$data(cols = grep("\\.response$", task$feature_names, value = TRUE))
    },
    weighted_average_prediction = function(task, weights, data) {
      wts = weights / sum(weights)
      response = as.matrix(data) %*% wts
      se = NULL
      PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), response = response, se = se)
    }
  ),
  private = list(
    .train = function(task) {
      data = self$prepare_data(task)
      n_weights = ncol(data)
      list("weights" = optimize_weights_learneravg(self, task, n_weights, data))
    },
    .predict = function(task) {
      self$weighted_average_prediction(task, self$model$weights, self$prepare_data(task))
    }
  )
)

# the following to avoid static checker warnings. This is because the '@usage' above checks for this.
# In fact we set usage only so that `pkgdown` shows the name correctly.
mlr_learners_regr.avg = NULL
mlr_learners_classif.avg = NULL

optimize_weights_learneravg = function(self, task, n_weights, data) {

      # objective function to optimize
      learneravg_objfun = function(x, task, measure, avg_weight_fun, data) {
        # This is the objective function we minimize using nlopt
        prd = avg_weight_fun(task, unlist(x), data)
        res = prd$score(measure)
        if (measure$minimize) res else -res
      }

      pars = self$param_set$get_values(tags = "train")
      pl = rep(list(p_dbl(0, 1)), length(data))
      names(pl) = names(data) %??% paste0("w.", seq_along(data))
      ps = do.call(ps, pl)
      optimizer = pars$optimizer
      if (inherits(optimizer, "character")) {
        optimizer = bbotk::opt(optimizer)
        if (inherits(optimizer, "OptimizerNLoptr") || inherits(optimizer, "OptimizerBatchNLoptr")) {
          optimizer$param_set$set_values(xtol_rel = 1e-8, algorithm = "NLOPT_LN_COBYLA", start_values = "center")
        }
      }
      measure = pars$measure
      if (is.character(measure)) measure = msr(measure)
      codomain = do.call(paradox::ps, structure(list(p_dbl(tags = ifelse(measure$minimize, "minimize", "maximize"))), names = measure$id))
      objfun = bbotk::ObjectiveRFun$new(
        fun = function(xs) learneravg_objfun(xs, task = task, measure = measure, avg_weight_fun = self$weighted_average_prediction, data = data),
        domain = ps, codomain = codomain
      )
      inst = bbotk::OptimInstanceSingleCrit$new(
        objective = objfun,
        terminator = bbotk::trm("combo", terminators = list(
          bbotk::trm("stagnation", iters = 20 * n_weights),
          bbotk::trm("evals", n_evals = 50 * n_weights)
        ))
      )
      lgr = lgr::get_logger("bbotk")
      old_threshold = lgr$threshold
      lgr2 = lgr::get_logger("mlr3/bbotk")
      old_threshold2 = lgr2$threshold
      on.exit({
        lgr$set_threshold(old_threshold)
        lgr2$set_threshold(old_threshold2)
      })
      lgr$set_threshold(self$param_set$values$log_level)
      lgr2$set_threshold(self$param_set$values$log_level)
      optimizer$optimize(inst)
      unlist(inst$result_x_domain)
}


# Check whether an object is a measure or convertable to one via `msr()`
check_measure = function(x, class = "Measure") {
  if (is.character(x)) {
    if (x %nin% mlr_measures$keys()) "Is a `character` but not a known measure" else TRUE
  } else {
    check_r6(x, class)
  }
}


# Check whether an object is an Optimizer or in a fixed set of optimizers.
check_optimizer = function(x, class = "Optimizer") {
  if (is.character(x)) {
    if (!(x %in% c("gensa", "nloptr", "random_search"))) {
      paste0("optimizer must be convertable to a bbotk::Optimizer via bbotk::opt().")
    } else {
      TRUE
    }
  } else {
    check_r6(x)
  }
}
