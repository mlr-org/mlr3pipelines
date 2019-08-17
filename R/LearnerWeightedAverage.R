#' @title Weighted Average of Features for Classification
#'
#' @aliases mlr_learners_classif.weightedaverage
#' @format [R6::R6Class()] inheriting from [mlr3::LearnerClassif].
#'
#' @description
#' Computes a weighted average of inputs.
#' Used in the context of computing weighted averages of predictions.
#'
#' If `weights.method` is "manual", the average is computed over weights provided by the user.
#' Predictions are averaged using `weights` (in order of appearance in the data); `weights` defaults to equal
#' weights for each feature.
#' For `weights.method`: "nloptr", nonlinear optimization from the package "nloptr" is used to optimize weights
#' for a measure provided in `measure` (defaults to `classif.acc`).
#' Learned weights can be obtained from `.$model`.
#' Using non-linear optimization is implemented in the SuperLearner R package.
#' For a more detailed analysis the reader is refered to
#' *LeDell, 2015: Scalable Ensemble Learning and Computationally Efficient Variance Estimation*.
#'
#'
#'
#' @section Parameter Set:
#' * `weights.method` :: `character(1)` \cr
#'   `manual` allows to supply weights, for `nloptr` weights are automatically determined using `nloptr`?
#'   Defaults to `manual`.
#' * `weights` :: `numeric(1)` \cr
#'   Numeric either of length 1 or the same length as the inputs. Defaults to 1.
#' * `measure` :: `character(1) | MeasureClassif` \cr
#'   Only for `weights.method = "nloptr"`. Measure to optimized weights for.
#'   The Measure is either obtained from `mlr_measures` or directly supplied. Defaults to `classif.acc`.
#' * `algorithm` :: `character(1)` \cr
#'   Several nonlinear optimization methods from `nloptr` are available.
#'   See `nloptr::nloptr.print.options()` for a list of possible options.
#'   Note that we only allow for derivative free local or global algorithms, i.e.
#'   NLOPT_(G|L)N_.
#'
#' @section Methods:
#' * `LearnerClassifWeightedAverage$new(), id = "classif.weightedavg")` \cr
#'   (`chr`) -> `self` \cr
#'   Constructor.
#' @family LearnerClassif
#' @include PipeOpEnsemble.R
#' @export
LearnerClassifWeightedAverage = R6Class("LearnerClassifWeightedAverage", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.weightedavg") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          params = list(
            ParamUty$new(id = "measure", tags = c("train", "required")),
            ParamFct$new(id = "algorithm", tags = c("train", "required"),
              levels = nlopt_levels)
          )
        ),
        param_vals = list(measure = "classif.acc", algorithm = "NLOPT_LN_COBYLA"),
        predict_types = c("response", "prob"),
        feature_types = c("integer", "numeric", "factor"),
        properties = c("twoclass", "multiclass")
      )
    },

    train_internal = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = self$prepare_data(task)
      n_weights = length(data)
      list("weights" = optimize_objfun_nlopt(task, pars, self$weighted_average_prediction, n_weights, data))
    },

    predict_internal = function(task) {
      self$weighted_average_prediction(task, self$model$weights, self$prepare_data(task))
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
        response = weighted_factor_mean(data, weights, task$class_names)
      } else {
        prob = weighted_matrix_sum(data, weights)
        prob = pmin(pmax(prob, 0), 1)
      }

      PredictionClassif$new(row_ids = task$row_ids, truth = task$truth(), response = response, prob = prob)
    }
  )
)

#' @title Weighted Average of Features for Regression
#'
#' @aliases mlr_learners_regr.weightedaverage
#' @format [R6::R6Class()] inheriting from [mlr3::LearnerRegr].
#'
#' @description
#' Computes a weighted average of inputs.
#' Used in the context of computing weighted averages of predictions.
#'
#' If `weights.method` is "manual", the average is computed over weights provided by the user.
#' Predictions are averaged using `weights` (in order of appearance in the data); `weights` defaults to equal
#' weights for each feature.
#' For `weights.method`: "nloptr", nonlinear optimization from package "nloptr" is used to optimize weights
#' for a measure provided as `measure` (defaults to `regr.mse`).
#' Learned weights can be obtained from `.$model`.
#'
#' @section Parameter Set:
#' * `weights.method` :: `character(1)` \cr
#'   `manual` allows to supply weights, for `nloptr` weights are automatically determined using `nloptr`?
#'   Defaults to `manual`.
#' * `weights` :: `numeric(1)` \cr
#'   Numeric either of length 1 or the same length as the inputs. Defaults to 1.
#' * `measure` :: `character(1) | MeasureClassif` \cr
#'   Only for `weights.method = "nloptr"`. Measure to optimized weights for.
#'   The Measure is either obtained from `mlr_measures` or directly supplied. Defaults to `classif.acc`.
#' * `algorithm` :: `character(1)` \cr
#'   Several nonlinear optimization methods from `nloptr` are available.
#'   See `nloptr::nloptr.print.options()` for a list of possible options.
#'   Note that we only allow for derivative free local or global algorithms, i.e.
#'   NLOPT_(G|L)N_*.
#'
#' @section Methods:
#' * `LearnerRegrWeightedAverage$new(), id = "regr.weightedavg")` \cr
#'   (`chr`) -> `self` \cr
#'   Constructor.
#' @family LearnerRegr
#' @include PipeOpEnsemble.R
#' @export
LearnerRegrWeightedAverage = R6Class("LearnerRegrWeightedAverage", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.weightedavg") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          params = list(
            ParamUty$new(id = "measure", tags = c("train", "required")),
            ParamFct$new(id = "algorithm", tags = c("train", "required"), levels = nlopt_levels)
          )
        ),
        param_vals = list(measure = "regr.mse", algorithm = "NLOPT_LN_COBYLA"),
        predict_types = "response",
        feature_types = c("integer", "numeric")
      )
    },

    train_internal = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = self$prepare_data(task)
      n_weights = ncol(data$response_matrix)
      list("weights" = optimize_objfun_nlopt(task, pars, self$weighted_average_prediction, n_weights, data))
    },

    predict_internal = function(task) {
      self$weighted_average_prediction(task, self$model$weights, self$prepare_data(task))
    },
    prepare_data = function(task) {
      response_matrix = as.matrix(task$data(cols = grep("\\.response$", task$feature_names, value = TRUE)))
      list(response_matrix = response_matrix)
    },
    weighted_average_prediction = function(task, weights, data) {
      wts = weights / sum(weights)

      response = c(data$response_matrix %*% wts)
      se = NULL
      PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), response = response, se = se)
    }
  )
)

nlopt_levels = c("NLOPT_GN_DIRECT", "NLOPT_GN_DIRECT_L", "NLOPT_GN_DIRECT_L_RAND",
  "NLOPT_GN_DIRECT_NOSCAL", "NLOPT_GN_DIRECT_L_NOSCAL", "NLOPT_GN_DIRECT_L_RAND_NOSCAL",
  "NLOPT_GN_ORIG_DIRECT", "NLOPT_GN_ORIG_DIRECT_L", "NLOPT_LN_PRAXIS",
  "NLOPT_GN_CRS2_LM", "NLOPT_GN_MLSL", "NLOPT_GN_MLSL_LDS",
  "NLOPT_LN_COBYLA", "NLOPT_LN_NEWUOA", "NLOPT_LN_NEWUOA_BOUND",
  "NLOPT_LN_NELDERMEAD", "NLOPT_LN_SBPLX", "NLOPT_LN_AUGLAG",
  "NLOPT_LN_AUGLAG_EQ", "NLOPT_LN_BOBYQA", "NLOPT_GN_ISRES")

nlopt_objfun = function(weights, task, measure, avg_weight_fun, data) {
  # This is the objective function we minimize using nlopt
  prd = avg_weight_fun(task, weights, data)
  res = prd$score(measure)
  if (measure$minimize) res else -res
}

optimize_objfun_nlopt = function(task, pars, avg_weight_fun, n_weights, data) {
  require_namespaces("nloptr")
  measure = assert_measure(pars$measure)

  opt = nloptr::nloptr(
    x0 = rep(1 / n_weights, n_weights),
    eval_f = nlopt_objfun,
    lb = rep(0, n_weights), ub = rep(1, n_weights),
    eval_g_ineq = function(x, task, measure, avg_weight_fun, data) max(x) - 1,
    opts = list(algorithm = pars$algorithm, xtol_rel = 1e-8),
    task = task,
    measure = measure,
    avg_weight_fun = avg_weight_fun,
    data = data
  )$solution
}

