#' @title PipeOpTuneThreshold
#'
#' @usage NULL
#' @name mlr_pipeops_tunethreshold
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Tunes optimal probability thresholds over different [`PredictionClassif`][mlr3::PredictionClassif]s.
#'
#' [`mlr3::Learner`] `predict_type`: `"prob"` is required.
#' Thresholds for each learner are optimized using the [`Optimizer`][bbotk::Optimizer] supplied via
#' the `param_set`.
#' Defaults to [`GenSA`][GenSA::GenSA].
#' Returns a single [`PredictionClassif`][mlr3::PredictionClassif].
#'
#' This PipeOp should be used in conjunction with [`PipeOpLearnerCV`] in order to
#' optimize thresholds of cross-validated predictions.
#' In order to optimize thresholds without cross-validation, use [`PipeOpLearnerCV`]
#' in conjunction with [`ResamplingInsample`][mlr3::ResamplingInsample].
#'
#' @section Construction:
#' ```
#' * `PipeOpTuneThreshold$new(id = "tunethreshold", param_vals = list())` \cr
#'   (`character(1)`, `list`) -> `self` \cr
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. Default: "tunethreshold".
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings
#'   that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOp`].
#'
#' @section State:
#' The `$state` is a named `list` with elements
#' * `thresholds` :: `numeric` learned thresholds
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOp`], as well as:
#'  * `measure` :: [`Measure`][mlr3::Measure]\cr
#'    [`Measure`][mlr3::Measure] | `character` to optimize.
#'    Will be converted to a [`Measure`][mlr3::Measure] in case it is `character`.
#'    Initialized to `"classif.ce"`, i.e. misclassification error.
#'  * `optimizer` :: [`Optimizer`][bbotk::Optimizer]|`character(1)`\cr
#'    [`Optimizer`][bbotk::Optimizer] used to find optimal thresholds.
#'    If `character`, converts to [`Optimizer`][bbotk::Optimizer]
#'    via [`opt`][bbotk::opt]. Initialized to [`OptimizerGenSA`][bbotk::OptimizerGenSA].
#'  * `log_level` :: `character(1)`|`integer(1)`\cr
#'    Set a temporary log-level for `lgr::get_logger("bbotk")`. Initialized to: "warn".
#'
#' @section Internals:
#' Uses the `optimizer` provided as a `param_val` in order to find an optimal threshold.
#' See the `optimizer` parameter for more info.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("learner_cv", lrn("classif.rpart", predict_type = "prob")) %>>%
#'   po("tunethreshold")
#'
#' task$data()
#' pop$train(task)
#'
#' pop$state
PipeOpTuneThreshold = R6Class("PipeOpTuneThreshold",
  inherit = PipeOp,

  public = list(
    initialize = function(id = "tunethreshold", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", custom_check = function(x)
          check_r6(if (is.character(x)) msr(x) else x, "MeasureClassif"), tags = "train"),
        ParamUty$new("optimizer", custom_check = function(x) {
          msg = paste0("'optimizer' needs to inherit from 'bbotk::Optimizer' or be convertable to one via bbotk::opt().")
          if (is.character(x)) {
            if (!(x %in% c("gensa", "nloptr", "random_search"))) {
              return(msg)
            }
          } else if (!inherits(x, "Optimizer")) {
            return(msg)
          }
          return(TRUE)
        }, tags = "train"),
        ParamUty$new("log_level", tags = "train",
          function(x) assert(check_string(x), check_integerish(x)))
      ))
      ps$values = list(measure = "classif.ce", optimizer = "gensa", log_level = "warn")
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "bbotk",
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction"),
        tags = "target transform"
      )
    },
    train = function(input) {
      if(!all(input[[1]]$feature_types$type == "numeric")) {
        stop("PipeOpTuneThreshold requires predicted probabilities! Set learner predict_type to 'prob'")
      }
      pred = private$.task_to_prediction(input[[1]])
      th = private$.optimize_objfun(pred)
      self$state = list("threshold" = th)
      return(list(NULL))
    },
    predict = function(input) {
      pred = private$.task_to_prediction(input[[1]])
      pred$set_threshold(self$state$threshold)
      return(list(pred))
    }
  ),
  private = list(
    .objfun = function(xs, pred, measure) {
      lvls = colnames(pred$prob)
      res = pred$set_threshold(unlist(xs))$score(measure)
      if (!measure$minimize) res = -res
      res
    },
    .optimize_objfun = function(pred) {
      optimizer = self$param_set$values$optimizer
      if (inherits(optimizer, "character")) optimizer = bbotk::opt(optimizer)
      ps = private$.make_param_set(pred)
      measure = self$param_set$values$measure
      if (is.character(measure)) measure = msr(measure) else measure
      objfun = bbotk::ObjectiveRFun$new(
        fun = function(xs) private$.objfun(xs, pred = pred, measure = measure),
        domain = ps
      )
      inst = bbotk::OptimInstanceSingleCrit$new(
        objective = objfun,
        terminator = bbotk::trm("combo", terminators = list(
          bbotk::trm("stagnation", iters = 20*ncol(pred$prob)),
          bbotk::trm("evals", n_evals = 50*ncol(pred$prob)))
        )
      )
      lgr = lgr::get_logger("bbotk")
      old_threshold = lgr$threshold
      lgr$set_threshold(self$param_set$values$log_level)
      optimizer$optimize(inst)
      on.exit(lgr$set_threshold(old_threshold))
      unlist(inst$result_x_domain)
    },
    .make_param_set = function(pred) {
      ps = ParamSet$new(params = list())
      for(cn in colnames(pred$prob))
        ps$add(ParamDbl$new(id = cn, lower = 0, upper = 1))
      return(ps)
    },
    .task_to_prediction = function(input) {
      prob = as.matrix(input$data(cols = input$feature_names))
      colnames(prob) = unlist(input$levels())
      PredictionClassif$new(input, row_ids = input$row_ids, truth = input$truth(),
        response = factor(colnames(prob)[max.col(prob, ties.method = "random")], levels = unlist(input$levels())),
        prob = prob)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("tunethreshold", PipeOpTuneThreshold)
