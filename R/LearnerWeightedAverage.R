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
#' @export
LearnerClassifWeightedAverage = R6Class("LearnerClassifWeightedAverage", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.weightedavg") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          params = list(
            ParamUty$new(id = "measure", default = "classif.acc", tags = "train"),
            ParamFct$new(id = "algorithm", default = "NLOPT_LN_COBYLA", tags = "train",
              levels = c("NLOPT_GN_DIRECT", "NLOPT_GN_DIRECT_L", "NLOPT_GN_DIRECT_L_RAND",
                         "NLOPT_GN_DIRECT_NOSCAL", "NLOPT_GN_DIRECT_L_NOSCAL", "NLOPT_GN_DIRECT_L_RAND_NOSCAL",
                         "NLOPT_GN_ORIG_DIRECT", "NLOPT_GN_ORIG_DIRECT_L", "NLOPT_LN_PRAXIS",
                         "NLOPT_GN_CRS2_LM", "NLOPT_GN_MLSL", "NLOPT_GN_MLSL_LDS",
                         "NLOPT_LN_COBYLA", "NLOPT_LN_NEWUOA", "NLOPT_LN_NEWUOA_BOUND",
                         "NLOPT_LN_NELDERMEAD", "NLOPT_LN_SBPLX", "NLOPT_LN_AUGLAG",
                         "NLOPT_LN_AUGLAG_EQ", "NLOPT_LN_BOBYQA", "NLOPT_GN_ISRES"))
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
      list("weights" = private$optimize_objfun_nlopt(task, pars))
    },

    predict_internal = function(task) {
      private$compute_weighted_average(task, self$model$weights)
    }
  ),

  private = list(
    compute_weighted_average = function(task, weights) {
      data = task$data(cols = task$feature_names)
      fcts = map_lgl(data, is.factor)

      # One-Hot encode factors
      data = map_dtc(data, function(x) {
        if(is.factor(x)) {
          if (length(levels(x)) > 2) {
            x = t(sapply(x, function(x) as.numeric(seq_len(length(levels(x))) %in% x)))
            colnames(x) = levels(x)
          } else {
            x = sapply(x, function(x) as.numeric(seq_len(length(levels(x)) - 1) %in% x))
          }
        }
        return(x)
      })

      # Compue weighted predictions
      if (task$class_n == 2L) {
        prob = as.matrix(data) %*% (weights / sum(weights))
        prob = pmin(prob, 1)
        prob = cbind(prob, 1 - prob)
      } else {
        # Either all inputs are response or all prob.
        assert_true(all(fcts) | !any(fcts))
        # Build n_weights matricies of dim (class_n, class_n) with
        # weights on the diag, allows for matrix-multiply
        wts_mat = map_dtc(
          split(weights / sum(weights), ceiling(seq_along(weights) / ifelse(all(fcts), 1L, task$class_n))),
          function(wt) {
            mat = matrix(0, nrow = task$class_n, ncol = task$class_n)
            diag(mat) = wt
            return(mat)
        })
        prob = as.matrix(data) %*% t(as.matrix(wts_mat))
      }

      colnames(prob) = task$class_names
      list(prob = prob)
    },
    objfun = function(weights, task, measure) {
      # This is the objective function we minimize using nlopt
      prd = invoke(self$new_prediction, row_ids = task$row_ids, truth = task$truth(), .args = private$compute_weighted_average(task, weights))
      res = prd$score(measure)
      if (!measure$minimize) res = -res
      return(res)
    },
    optimize_objfun_nlopt = function(task, pars) {
      requireNamespace("nloptr")

      if (is.character(pars$measure)) {
        assert_true(mlr_measures$has(pars$measure))
        measure = mlr_measures$get(pars$measure)
      } else {
        measure = assert_measure(pars$measure)
      }
      n_weights = length(task$feature_names)

      opt = nloptr::nloptr(
        x0 = rep(1 / n_weights, n_weights),
        eval_f = private$objfun,
        lb = rep(0, n_weights), ub = rep(1, n_weights),
        eval_g_ineq = function(x, task, measure) {max(x) - 1},
        opts = list(algorithm = pars$algorithm, xtol_rel = 1.0e-8),
        task = task,
        measure = measure
      )
      return(opt$solution)
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
#'
#' @section Parameter Set:
#' * `weights.method` :: `character(1)` \cr
#'   `manual` allows to supply weights, for `nloptr` weights are automatically determined using `nloptr`?
#'   Defaults to `manual`.
#' * `weights` :: `numeric(1)` \cr
#'   Numeric either of length 1 or the same length as the inputs. Defaults to 1.
#' * `est.se` :: `logical(1)` \cr
#'   Should the standard deviation between different models be estimated?
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
#' @export
LearnerRegrWeightedAverage = R6Class("LearnerRegrWeightedAverage", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.weightedavg") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          params = list(
            ParamLgl$new(id = "est.se", default = TRUE, tags = "train"),
            ParamUty$new(id = "measure", default = "classif.acc", tags = "train"),
            ParamFct$new(id = "algorithm", default = "NLOPT_LN_COBYLA", tags = "train",
              levels = c("NLOPT_GN_DIRECT", "NLOPT_GN_DIRECT_L", "NLOPT_GN_DIRECT_L_RAND",
                         "NLOPT_GN_DIRECT_NOSCAL", "NLOPT_GN_DIRECT_L_NOSCAL", "NLOPT_GN_DIRECT_L_RAND_NOSCAL",
                         "NLOPT_GN_ORIG_DIRECT", "NLOPT_GN_ORIG_DIRECT_L", "NLOPT_LN_PRAXIS",
                         "NLOPT_GN_CRS2_LM", "NLOPT_GN_MLSL", "NLOPT_GN_MLSL_LDS",
                         "NLOPT_LN_COBYLA", "NLOPT_LN_NEWUOA", "NLOPT_LN_NEWUOA_BOUND",
                         "NLOPT_LN_NELDERMEAD", "NLOPT_LN_SBPLX", "NLOPT_LN_AUGLAG",
                         "NLOPT_LN_AUGLAG_EQ", "NLOPT_LN_BOBYQA", "NLOPT_GN_ISRES"))
          )
        ),
        param_vals = list(measure = "regr.mse", algorithm = "NLOPT_LN_COBYLA"),
        predict_types = c("response", "se"),
        feature_types = c("integer", "numeric")
      )
    },

    train_internal = function(task) {
      pars = self$param_set$get_values(tags = "train")
      list("weights" = private$optimize_objfun_nlopt(task, pars))
    },

    predict_internal = function(task) {
      private$compute_weighted_average(task, self$model$weights)
    }
  ),

  private = list(
    compute_weighted_average = function(task, weights) {
      wts = weights / sum(weights)
      data_response = task$data(cols = grep("\\.response$", task$feature_names, value = TRUE))
      wt_avg = as.matrix(data_response) %*% wts

      if (self$predict_type == "se") {
        data_se = task$data(cols = grep("\\.se$", task$feature_names, value = TRUE))
        if (ncol(data_se) == 0) {
          se = rep(0, nrow(data_response))
        } else {
          se = sqrt(as.matrix(data_se)^2 %*% wts)
        }
        if (self$pars$est.se)
          se = se + apply(data_response, 1, function(x, wt) {sqrt(sum(wt *(x-weighted.mean(x, wt))^2)*(sum(wt)/(sum(wt)^2-sum(wt^2))))}, wt = wts)
      } else {
        se = NULL
      }
      list(response = wt_avg, se = se)
    },
    objfun = function(weights, task, measure) {
      # This is the objective function we minimize using nlopt
      prd = invoke(self$new_prediction, row_ids = task$row_ids, truth = task$truth(), .args = private$compute_weighted_average(task, weights))
      res = prd$score(measure)
      if (!measure$minimize) res = -res
      return(res)
    },
    optimize_objfun_nlopt = function(task, pars) {
      requireNamespace("nloptr")

      if (is.character(pars$measure)) {
        assert_true(mlr_measures$has(pars$measure))
        measure = mlr_measures$get(pars$measure)
      } else {
        measure = assert_measure(pars$measure)
      }
      n_weights = length(grep("\\.response$", task$feature_names, value = TRUE))

      opt = nloptr::nloptr(
        x0 = rep(1 / n_weights, n_weights),
        eval_f = private$objfun,
        lb = rep(0, n_weights), ub = rep(1, n_weights),
        eval_g_ineq = function(x, task, measure) {max(x) - 1},
        opts = list(algorithm = pars$algorithm, xtol_rel = 1.0e-8),
        task = task,
        measure = measure
      )
      return(opt$solution)
    }
  )
)

Ensemble = R6Class("Ensemble",
  public = list(
    initialize = function() {
    },
    weighted_avg_regression_from_predictions = function(inputs, weights) {
      map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
      row_ids = inputs[[1]]$row_ids
      truth = inputs[[1]]$truth
      weighted_avg_regression(row_ids, truth, inputs, weights)
    },
    weighted_avg_regression = function(row_ids, truth, inputs, weights) {
      weights = weights / sum(weights)
      responsematrix = simplify2array(map(inputs, "response"))
      response = c(responsematrix %*% weights)
      se = NULL
      if (all(map_lgl(inputs, function(x) "se" %in% names(x$data)))) {
        se = c(sqrt(
          (
            simplify2array(map(inputs, "se"))^2 +
            (responsematrix - response)^2
          ) %*% weights
        ))
      }
      PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se)
    },
    weighted_avg_classification = function(inputs, weights) {
      row_ids = inputs[[1]]$row_ids
      map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
      truth = inputs[[1]]$truth

      weights = weights / sum(weights)

      # Drop zero-weights for efficiency
      inputs = inputs[weights != 0]
      weights = weights[weights != 0]

      has_probs = all(map_lgl(inputs, function(x) {
        !is.null(x$prob)
      }))
      has_response = all(map_lgl(inputs, function(x) {
        !is.null(x$response)
      }))
      if (!(has_probs || has_response)) {
        stop("PipeOpMajorityVote input predictions had missing 'prob' and missing 'response' values. At least one of them must be given for all predictions.")
      }
      prob = response = NULL
      if (has_probs) {
        prob = private$weighted_prob_avg(inputs, weights)
      }
      if (has_response) {
        response = private$weighted_majority_vote(inputs, weights)
      }
      PredictionClassif$new(row_ids = row_ids, truth = truth, response = response, prob = prob)
    },
    weighted_majority_vote = function(inputs, weights) {
      alllevels = levels(inputs[[1]]$response)
      map(inputs, function(x) assert_true(identical(sort(alllevels), sort(levels(x$response)))))

      accmat = matrix(0, nrow = length(inputs[[1]]$response), ncol = length(alllevels))
      for (idx in seq_along(inputs)) {
        rdf = data.frame(x = factor(inputs[[idx]]$response, levels = alllevels))
        curmat = model.matrix(~ 0 + x, rdf) * weights[idx]
        accmat = accmat + curmat
      }
      factor(alllevels[max.col(accmat)], levels = alllevels)
    },
    weighted_prob_avg = function(inputs, weights) {
      alllevels = colnames(inputs[[1]]$prob)
      assert_character(alllevels, any.missing = FALSE)
      map(inputs, function(x) assert_true(identical(sort(alllevels), sort(colnames(x$prob)))))

      accmat = inputs[[1]]$prob * weights[1]
      for (idx in seq_along(inputs)[-1]) {
        accmat = accmat + inputs[[idx]]$prob[, alllevels, drop = FALSE] * weights[idx]
      }
      accmat
    }
  )
)

