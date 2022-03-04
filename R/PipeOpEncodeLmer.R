#' @title Impact Encoding with Random Intercept Models
#'
#' @usage NULL
#' @name mlr_pipeops_encodelmer
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Encodes columns of type `factor`, `character` and `ordered`.
#'
#' PipeOpEncodeLmer() converts factor levels of each factorial column to the
#' estimated coefficients of a simple random intercept model.
#' Models are fitted with the glmer function of the lme4 package and are
#' of the type `target ~ 1 + (1 | factor)`.
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
#' PipeOpEncodeLmer$new(id = "encodelmer", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encodelmer"`.
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
#' * `fast_optim`  :: `logical(1)` \cr
#'   Initialized to `TRUE`.
#'   If "fast_optim" is `TRUE` (default), a faster (up to 50 percent)
#'   optimizer from the nloptr package is used when fitting the lmer models.
#'   This uses additional stopping criteria which can give suboptimal results.
#'
#' @section Internals:
#' Uses the [`lme4::glmer`]. This is relatively inefficient for features with a large number of levels.
#'
#' @section Methods:
#' Only methods inherited [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#' poe = po("encodelmer")
#'
#' task = TaskClassif$new("task",
#'   data.table::data.table(
#'     x = factor(c("a", "a", "a", "b", "b")),
#'     y = factor(c("a", "a", "b", "b", "b"))),
#'   "x")
#'
#' poe$train(list(task))[[1]]$data()
#'
#' poe$state
PipeOpEncodeLmer = R6Class("PipeOpEncodeLmer",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "encodelmer", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamLgl$new("fast_optim", tags = c("train", "required"))
      ))
      ps$values = list(fast_optim = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = c("lme4", "nloptr"), tags = "encode", feature_types = c("factor", "ordered"),
        label = "Impact Encoding with Random Intercept Models", man = "mlr3pipelines::mlr_pipeops_encodelmer")
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      task_type = if (is.numeric(target)) "regr" else "classif"
      state = list()
      # for prediction, use complete encoding model
      # different funs depending on task.type / multi/binaryclass

      state$target_levels = levels(target)

      # one vs rest for multiclass.
      if (length(state$target_levels) <= 2) {
        state$control = lapply(dt, function(col) {
          private$fit_lmer(col, target, self$param_set$values$fast_optim, task_type)
        })
      } else {
        # create list with binary "one vs. rest" target variables
        bin_targets = sapply(levels(target), function(x) factor(x == target),
          simplify = FALSE)
        # for prediction, use complete encoding model
        state$control = sapply(colnames(dt), function(cname) {
          sapply(state$target_levels, function(lvl) {
            private$fit_lmer(dt[[cname]], bin_targets[[lvl]], self$param_set$values$fast_optim, task_type)
            }, simplify = FALSE)
          }, simplify = FALSE)
      }
      # FIXME: We currently do not implement cross-validated encodings
      state
    },

    .transform_dt = function(dt, levels) {
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
      dt_new
    },
    fit_lmer = function(feature, target, fast_optim, task_type) {
      args = private$get_args_nlopt_lmer(feature, target, fast_optim, task_type)
      if (task_type == "classif") args$family = stats::binomial
      # lmer for regr, glmer for classif
      if (task_type == "regr") {
        mod = invoke(lme4::lmer, .args = args, .opts = list(warnPartialMatchArgs = FALSE, warnPartialMatchDollar = FALSE))
      } else {
        mod = invoke(lme4::glmer, .args = args, .opts = list(warnPartialMatchArgs = FALSE, warnPartialMatchDollar = FALSE))
      }
      private$get_coefs(mod)
    },
    get_args_nlopt_lmer = function(feature, target, fast_optim, task_type) {

      # lmer for regr, glmer for classif
      if (task_type == "regr") {
        control_fun = lme4::lmerControl
      } else {
        control_fun = lme4::glmerControl
      }

      # nloptwrap for fast optim
      if (fast_optim) {
        control = control_fun()
      } else {
        control = control_fun(optimizer = "nloptwrap", calc.derivs = FALSE)
      }

      list(formula = y ~ 1 + (1 | lvl),
        data = data.frame(lvl = feature, y = target),
        na.action = stats::na.omit, control = control)
    },
    get_coefs = function(mod) {
      coefs = invoke(stats::coef, mod, .opts = list(warnPartialMatchArgs = FALSE, warnPartialMatchDollar = FALSE))$lvl
      lvls = rownames(coefs)
      coefs = coefs[,1]
      names(coefs) = lvls
      intercept = unname(lme4::fixef(mod))
      # replace missing coefs with intercept value
      coefs[is.na(coefs)] = intercept
      # save intercept value for new levels during prediction
      c(coefs, ..new..level.. = intercept)
    }
  )
)

mlr_pipeops$add("encodelmer", PipeOpEncodeLmer)

# # performance tips from https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
# nlopt <- function(par, fn, lower, upper, control) {
# .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper,
#     opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
#     maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
#   list(par = res$solution, fval = res$objective,
#    conv = if (res$status > 0) 0 else res$status, message = res$message)
# }
