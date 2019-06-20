#' @title Weighted Average of Features
#'
#' @aliases mlr_learners_classif.weightedaverage
#' @format [R6::R6Class()] inheriting from [mlr3::LearnerClassif].
#'
#' @description
#' Computes a weighted average of the features.
#' If `weights.method` is "manual", the average is computed over weights provided by the user, defaulting
#' Predictions are averaged using `weights` (in order of appearance in the data); `weights` defaults to equal
#' weights for each feature.
#'
#' For `weights.method`: "nloptr", nonlinear optimization from the package "nloptr" is used to optimize weights
#' for a measure provided in `measure` (defaults to `classif.acc`).
#' 
#' Several nonlinear optimization methods from `nloptr` can be chosen.
#' See `nloptr::nloptr.print.options()` for a list of possible options.
#' This can be set via the `algorithm` parameter.
#' 
#' @section Methods:
#' * `LearnerClassifWeightedAverage$new(), id = "wtaverage")` \cr
#'   (`chr`) -> `self` \cr
#'   Constructor.
#' @family LearnerClassif
#' @include PipeOp.R
#' @export
LearnerClassifWeightedAverage = R6Class("LearnerClassifWeightedAverage", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.weightedavg") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "weights.method", default = "manual", levels = c("manual", "nloptr"), tags = "train"),
            ParamUty$new(id = "weights", default = 1L, tags = "train"),
            ParamUty$new(id = "measure", default = "classif.acc", tags = "train"),
            ParamFct$new(id = "algorithm", default = "NLOPT_LN_COBYLA", levels = c("NLOPT_LN_COBYLA", "NLOPT_LN_BOBYQA"), tags = "train")
          )
        ),
        param_vals = list(weights.method = "manual", weights = 1L, measure = "classif.acc", algorithm = "NLOPT_LN_COBYLA"),
        predict_types = c("response", "prob"),
        feature_types = c("integer", "numeric", "factor"),
        properties = c("twoclass", "multiclass")
      )
    },

    train = function(task) {
      pars = self$params("train")
      if (pars$weights.method == "manual") {
        assert_numeric(pars$weights, lower = 0L)
        assert_true(length(pars$weights) == 1L | length(pars$weights) == length(task$feature_names))
        assert_true(sum(pars$weights) > 0)
        if (length(pars$weights) == 1L) weights = rep(pars$weights, length(task$feature_names))
        else weights = pars$weights
        self$model = list("weights" = weights)
      } else {
        self$model = list("weights" = private$optimize_objfun_nlopt(task, pars))
      }
      invisible(self)
    },

    predict = function(task) {
      pars = self$params("predict")
      # newdata = as.matrix(task$data(cols = task$feature_names))
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
      as_prediction_data(task, prob = prob)
    },
    objfun = function(weights, task, measure) {
      # This is the objective function we minimize using nlopt
      prd = new_prediction(task, private$compute_weighted_average(task, weights))
      res = measure$calculate(prediction = prd)
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




