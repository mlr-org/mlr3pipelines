#' @title Optimized Weighted Average of Features for Classification and Regression
#'
#' @usage mlr_learners_classif.avg
#' @name mlr_learners_avg
#' @aliases mlr_learners_classif.avg
#' @format [`R6Class`] object inheriting from [`mlr3::LearnerClassif`]/[`mlr3::Learner`].
#'
#' @description
#' Computes a weighted average of inputs.
#' Used in the context of computing weighted averages of predictions.
#'
#' Predictions are averaged using `weights` (in order of appearance in the data) which are optimized using
#' nonlinear optimization from the package "nloptr" for a measure provided in `measure` (defaults to `classif.acc`
#' for `LearnerClassifAvg` and `regr.mse` for `LearnerRegrAvg`).
#' Learned weights can be obtained from `$model`.
#' Using non-linear optimization is implemented in the SuperLearner R package.
#' For a more detailed analysis the reader is referred to LeDell (2015).
#'
#' Note, that weights always sum to 1 by dividing through sum(weights) before weighting
#' incoming features.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`LearnerClassif`], as well as:
#'  * `measure` :: [`Measure`][mlr3::Measure] | `character` \cr
#'    [`Measure`][mlr3::Measure] to optimize for.
#'    Will be converted to a [`Measure`][mlr3::Measure] in case it is `character`.
#'    Initialized to `"classif.ce"`, i.e. misclassification error for classification
#'    and `"regr.mse"`, i.e. mean squared error for regression.
#'  * `optimizer` :: [`Optimizer`][bbotk::Optimizer] | `character(1)`\cr
#'    [`Optimizer`][bbotk::Optimizer] used to find optimal thresholds.
#'    If `character`, converts to [`Optimizer`][bbotk::Optimizer]
#'    via [`opt`][bbotk::opt]. Initialized to [`OptimizerNLoptr`][bbotk::OptimizerNLoptr].
#'    Nloptr hyperparameters are initialized to `xtol_rel = 1e-8`, `algorithm = "NLOPT_LN_COBYLA"`
#'    and equal initial weights for each learner.
#'    For more fine-grained control, it is recommended to supply a instantiated [`Optimizer`][bbotk::Optimizer].
#'  * `log_level` :: `character(1)` | `integer(1)`\cr
#'    Set a temporary log-level for `lgr::get_logger("bbotk")`. Initialized to: "warn".
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
#' `r tools::toRd(bibentries["ledell_2015"])`
#'
#' @family Learners
#' @family Ensembles
#' @include PipeOpEnsemble.R
#' @export
LearnerClassifAvg = R6Class("LearnerClassifAvg", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.avg") {
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", custom_check = check_class_or_character("MeasureClassif", mlr_measures), tags = "train"),
        ParamUty$new("optimizer", custom_check = check_optimizer, tags = "train"),
        ParamUty$new("log_level", tags = "train",
          function(x) check_string(x) %check||% check_integerish(x))
      ))
      ps$values = list(measure = "classif.ce", optimizer = "nloptr", log_level = "warn")
      super$initialize(
        id = id,
        param_set = ps,
        predict_types = c("response", "prob"),
        feature_types = c("integer", "numeric", "factor"),
        properties = c("twoclass", "multiclass")
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
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", custom_check = check_class_or_character("MeasureRegr", mlr_measures), tags = "train"),
        ParamUty$new("optimizer", custom_check = check_optimizer, tags = "train"),
        ParamUty$new("log_level", tags = "train",
          function(x) check_string(x) %check||% check_integerish(x))
      ))
      ps$values = list(measure = "regr.mse", optimizer = "nloptr", log_level = "warn")
      super$initialize(
        id = id,
        param_set = ps,
        predict_types = "response",
        feature_types = c("integer", "numeric")
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
      ps = ParamSet$new(params = imap(data, function(x, n) {
        if (is.numeric(n)) n = paste0("w.", n)
        ParamDbl$new(id = n, lower = 0, upper = 1)
      }))
      optimizer = pars$optimizer
      if (inherits(optimizer, "character")) {
        optimizer = bbotk::opt(optimizer)
        if (inherits(optimizer, "OptimizerNLoptr")) {
          optimizer$param_set$values = list(xtol_rel = 1e-8, algorithm = "NLOPT_LN_COBYLA", x0 = rep(1/n_weights, n_weights))
        }
      }
      measure = pars$measure
      if (is.character(measure)) measure = msr(measure)
      objfun = bbotk::ObjectiveRFun$new(
        fun = function(xs) learneravg_objfun(xs, task = task, measure = measure, avg_weight_fun = self$weighted_average_prediction, data = data),
        domain = ps
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
      on.exit(lgr$set_threshold(old_threshold))
      lgr$set_threshold(self$param_set$values$log_level)
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
