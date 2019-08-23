#' @title Impact Encoding with Random Intercept Models
#'
#' @usage NULL
#' @name mlr_pipeops_encode
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes columns of type `factor`, `character` and `ordered`.
#'
#' PipeOpEncodeLmer() converts factor levels of each factorial column to the
#' estimated coefficients of a simple random intercept model.
#' Models are fitted with the glmer function of the lme4 package and are
#' of the type \code{target ~ 1 + (1 | factor)}.
#' If the task is a regression task, the numeric target
#' variable is used as dependent variable and the factor is used for grouping.
#' If the task is a classification task, the target variable is used as dependent variable
#' and the factor is used for grouping.
#' If the target variable is multiclass, for each level of the multiclass target variable,
#' binary "one vs. rest" models are fitted.
#'
#' For training, multiple models can be estimated in a cross-validation scheme
#' to ensure that the same factor level does not always result in identical
#' values in the converted numerical feature.
#' For prediction, a global model (which was fitted on all observations
#' during training) is used for each factor.
#' New factor levels are converted to the value of the intercept coefficient
#' of the global model for prediction.
#' NAs are ignored by the CPO.
#'
#' Use the [`PipeOpTaskPreproc`] `$affect_columns` functionality to only encode a subset of
#' columns, or only encode columns of a certain type.
#'
#' @section Construction:
#' ```
#' PipeOpEncodeLmer$new(id = "encode", param_vals = list())
#' ```
#" * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encode"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would
#'   otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected `factor`, `character` or
#' `ordered` parameters encoded according to the `method` parameter.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `target_levels` :: `character`\cr
#'   Levels of the target columns.
#' * `control` :: a named `list`\cr
#'   List of coefficients learned via `glmer`
#'
#' @section Parameters:
#' * `fast.optim`  :: `logical(1)` \cr
#'   Initialized to `TRUE`.
#'   If \dQuote{fast.optim} is \code{TRUE} (default), a faster (up to 50 percent)
#'   optimizer from the nloptr package is used when fitting the lmer models.
#'   This uses additional stopping criteria which can give suboptimal results.
#'
#' @section Internals:
#' Uses the [`lme4::glmer`]. This is relatively inefficient for features with a large number of levels.
#'
#' @section Methods:
#' Only methods inherited [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' poe = mlr_pipeops$get("encodeLmer")
#'
#' task = mlr3::TaskClassif$new("task",
#'   data.table::data.table(x = letters[1:3], y = letters[1:3]), "x")
#'
#' poe$train(list(task))[[1]]$data()
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpEncodeLmer = R6Class("PipeOpEncodeLmer",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "encodeLmer", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamLgl$new("fast.optim", tags = c("train"))
      ))
      ps$values = list(fast.optim = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "lme4")
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("factor", "ordered", "character"), get("id")]
    },

    train_task = function(task) {
      dt_columns = self$select_cols(task)
      cols = dt_columns
      if (!length(cols)) {
        self$state = list(dt_columns = dt_columns)
        return(task)
      }
      dt = task$data(cols = cols)
      dt = as.data.table(self$get_state_dt(dt, task$truth(), task$task_type))
      self$state$dt_columns = dt_columns
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    predict_task = function(task) {
      cols = self$state$dt_columns
      if (!length(cols)) {
        return(task)
      }
      dt = task$data(cols = cols)
      dt = as.data.table(self$transform_dt(dt))
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    get_state_dt = function(dt, target, task_type) {

      # for prediction, use complete encoding model
      # different funs depending on task.type / multi/binaryclass
      self$state$target_levels = levels(target)

      # --- fit lmer models ---
      fit_lmer = self$get_fit_lmer_fun(target, task_type)
      if (length(self$state$target_levels) <= 2) {
        self$state$control = lapply(dt, function(col) {
          fit_lmer(col, target, self$param_set$values$fast.optim)
        })
      } else {
        # create list with binary "one vs. rest" target variables
        bin_targets = sapply(levels(target), function(x) factor(x == target),
          simplify = FALSE)
        # for prediction, use complete encoding model
        self$state$control = sapply(colnames(dt), function(cname) {
          sapply(self$state$target_levels, function(lvl) {
            fit_lmer(dt[[cname]], bin_targets[[lvl]], self$param_set$values$fast.optim)
            }, simplify = FALSE)
          }, simplify = FALSE)
      }
      # FIXME: We currently do not implement cross-validated encodings
      self$transform_dt(dt)
    },

    transform_dt = function(dt) {
      if (length(self$state$target_levels) <= 2) {
        dt_new = map_dtc(colnames(dt), function(cname) {
          as.numeric(self$state$control[[cname]][as.character(dt[[cname]])])
        })
        colnames(dt_new) = colnames(dt)
      } else {
        num_vals_list = sapply(colnames(dt), function(cname) {
          sapply(self$state$target_levels, function(lvl) {
            as.numeric(self$state$control[[cname]][[lvl]][as.character(dt[[cname]])])
          }, simplify = FALSE)
        }, simplify = FALSE)
        # return df with new feature names: feature_name.target_level
        # NOTE: special symbols in target levels (e.g. "-") are transformed to "."
        # by as.data.frame to allign with naming rules for data.frame columns
        dt_new = as.data.frame(num_vals_list, row.names = rownames(dt))
      }
      return(dt_new)
    },

    get_fit_lmer_fun = function(target, task_type) {
      if (task_type == "regr") return(fit_lmer_regr)
      else if (task_type == "classif") {
        if (length(levels(target)) == 2) return(fit_lmer_binaryclass)
        else return(fit_lmer_multiclass)
      } else {
        stopf("Not implemented for task type %s!", task_type)
      }
    }
  )
)

mlr_pipeops$add("encodeLmer", PipeOpEncodeLmer)


# heavily influenced by the embed package
fit_lmer_regr = function(feature, target, fast.optim) {
  # performance tips from https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
  nlopt <- function(par, fn, lower, upper, control) {
  .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper,
    opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
    maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
  list(par = res$solution,
    fval = res$objective,
    conv = if (res$status > 0) 0 else res$status,
    message = res$message)
  }
  args = list(formula = y ~ 1 + (1 | lvl),
    data = data.frame(lvl = feature, y = target),
    na.action = na.omit,
    control = if (fast.optim) {
      lme4::lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
    } else {
      lme4::lmerControl()
    })
  mod = do.call(lme4::lmer, args)
  coefs = coef(mod)$lvl
  lvls = rownames(coefs)
  coefs = coefs[,1]
  names(coefs) = lvls
  intercept = unname(lme4::fixef(mod))
  # replace missing coefs with intercept value
  coefs[is.na(coefs)] = intercept
  # save intercept value for new levels during prediction
  coefs = c(coefs, ..new..level.. = intercept)
  coefs
}

# heavily influenced by the embed package
fit_lmer_binaryclass = function(feature, target, fast.optim) {
  # performance tips from https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
  nlopt <- function(par, fn, lower, upper, control) {
  .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper,
    opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
    maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
  list(par = res$solution,
    fval = res$objective,
    conv = if (res$status > 0) 0 else res$status,
    message = res$message)
  }
  args = list(formula = y ~ 1 + (1 | lvl),
    data = data.frame(lvl = feature, y = target),
    family = stats::binomial,
    na.action = na.omit,
    control = if (fast.optim) {
      lme4::glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
    } else {
      lme4::glmerControl()
    })
  mod = do.call(lme4::glmer, args)
  coefs = coef(mod)$lvl
  lvls = rownames(coefs)
  coefs = coefs[,1]
  names(coefs) = lvls
  intercept = unname(lme4::fixef(mod))
  # replace missing coefs with intercept value
  coefs[is.na(coefs)] = intercept
  # save intercept value for new levels during prediction
  coefs = c(coefs, ..new..level.. = intercept)
  coefs
}

# heavily influenced by the embed package
fit_lmer_multiclass = function(feature, target, fast.optim) {
  # performance tips from https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
  nlopt <- function(par, fn, lower, upper, control) {
  .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper,
    opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
    maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
  list(par = res$solution,
    fval = res$objective,
    conv = if (res$status > 0) 0 else res$status,
    message = res$message)
  }
  args = list(formula = y ~ 1 + (1 | lvl),
    data = data.frame(lvl = feature, y = target),
    family = stats::binomial,
    na.action = na.omit,
    control = if (fast.optim) {
      lme4::glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
    } else {
      lme4::glmerControl()
    })
  mod = do.call(lme4::glmer, args)
  coefs = coef(mod)$lvl
  lvls = rownames(coefs)
  coefs = coefs[,1]
  names(coefs) = lvls
  intercept = unname(lme4::fixef(mod))
  # replace missing coefs with intercept value
  coefs[is.na(coefs)] = intercept
  # save intercept value for new levels during prediction
  coefs = c(coefs, ..new..level.. = intercept)
  coefs
}
