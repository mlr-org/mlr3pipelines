#' @title Tune the Threshold of a Classification Prediction
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
#'  * `measure` :: [`Measure`][mlr3::Measure]  | `character`\cr
#'    [`Measure`][mlr3::Measure] to optimize for.
#'    Will be converted to a [`Measure`][mlr3::Measure] in case it is `character`.
#'    Initialized to `"classif.ce"`, i.e. misclassification error.
#'  * `optimizer` :: [`Optimizer`][bbotk::Optimizer]|`character(1)`\cr
#'    [`Optimizer`][bbotk::Optimizer] used to find optimal thresholds.
#'    If `character`, converts to [`Optimizer`][bbotk::Optimizer]
#'    via [`opt`][bbotk::opt]. Initialized to [`OptimizerGenSA`][bbotk::OptimizerGenSA].
#'  * `log_level` :: `character(1)` | `integer(1)`\cr
#'    Set a temporary log-level for `lgr::get_logger("bbotk")`. Initialized to: "warn".
#'
#' @section Internals:
#' Uses the `optimizer` provided as a `param_val` in order to find an optimal threshold.
#' See the `optimizer` parameter for more info.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
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
#' @family PipeOps
#' @template seealso_pipeopslist
#' @export
PipeOpTuneThreshold = R6Class("PipeOpTuneThreshold",
  inherit = PipeOp,

  public = list(
    initialize = function(id = "tunethreshold", param_vals = list()) {
      ps = ps(
        measure = p_uty(custom_check = check_class_or_character("Measure", mlr_measures), tags = "train"),
        optimizer = p_uty(custom_check = check_optimizer, tags = "train"),
        log_level = p_uty(tags = "train",
          function(x) check_string(x) %check||% check_integerish(x))
      )
      ps$values = list(measure = "classif.ce", optimizer = "gensa", log_level = "warn")
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "bbotk",
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction"),
        tags = "target transform"
      )
    }
  ),
  active = list(
    predict_type = function() "response"  # we are predict type "response" for now, so we don't break things. See discussion in #712
  ),
  private = list(
    .train = function(input) {
      if(!all(input[[1]]$feature_types$type == "numeric")) {
        stop("PipeOpTuneThreshold requires predicted probabilities! Set learner predict_type to 'prob'")
      }
      pred = private$.task_to_prediction(input[[1]])
      th = private$.optimize_objfun(pred)
      self$state = list("threshold" = th)
      return(list(NULL))
    },
    .predict = function(input) {
      pred = private$.task_to_prediction(input[[1]])
      pred$set_threshold(self$state$threshold)
      return(list(pred))
    },
    .objfun = function(xs, pred, measure) {
      lvls = colnames(pred$prob)
      res = pred$set_threshold(unlist(xs))$score(measure)
      if (!measure$minimize) res = -res
      return(setNames(list(res), measure$id))
    },
    .optimize_objfun = function(pred) {
      optimizer = self$param_set$values$optimizer
      if (inherits(optimizer, "character")) optimizer = bbotk::opt(optimizer)
      if (inherits(optimizer, "OptimizerGenSA")) optimizer$param_set$values$trace.mat = TRUE  # https://github.com/mlr-org/bbotk/issues/214
      ps = private$.make_param_set(pred)
      measure = self$param_set$values$measure
      if (is.character(measure)) measure = msr(measure) else measure
      codomain = do.call(paradox::ps, structure(list(p_dbl(tags = ifelse(measure$minimize, "minimize", "maximize"))), names = measure$id))

      objfun = bbotk::ObjectiveRFun$new(
        fun = function(xs) private$.objfun(xs, pred = pred, measure = measure),
        domain = ps, codomain = codomain
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
      on.exit(lgr$set_threshold(old_threshold))
      lgr$set_threshold(self$param_set$values$log_level)
      optimizer$optimize(inst)
      unlist(inst$result_x_domain)
    },
    .make_param_set = function(pred) {
      pset = setNames(map(colnames(pred$prob), function(x) p_dbl(0,1)), colnames(pred$prob))
      mlr3misc::invoke(paradox::ps, .args = pset)
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
