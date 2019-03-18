#' @title PipeOpPredPostproc
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Base class for handling most "postprocessing" operations on predictions.
#' These are operations that have exactly one prediction object as input and one
#' prediction object as output.
#'
#' Users must implement `$train()` and `$predict()`, which have a [`Prediction`]
#' input and should return that `Prediction`. The `Prediction` should, if possible, be
#' manipulated in-place, and should not be cloned.
#'
#' @section Methods:
#' * `PipeOpPredPostproc$new(id, param_set = ParamSet$new())` \cr
#'   (`character(1)`, `ParamSet`, `logical(1)`) -> `self` \cr
#'   Constructor.
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpPredPostproc = R6Class("PipeOpPredPostproc",
  inherit = PipeOp,

  public = list(
    threshold = NULL,
    measure = NULL,
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), packages = character(0)) {
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = packages,
        input = data.table(name = rep_suffix("input", innum), train = "NULL", predict = "Prediction"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
    },
    train = function(input) {
      self$state = list()
      list(NULL)
    },
    predict = function(input) stop("abstract")
  ),
  private = list(
    objfun = function(input) {
      e = list("prediction" = input)
      res = self$measure$calculate(e)
      if (!self$measure$minimize) res = -res
      res
    },
    optimize_objfun_gensa = function(input) { # 
      requireNamespace("GenSA")
      nclass = ncol(input$prob)
      cnames = colnames(input$prob)
      init_threshold = rep(1 / nclass, nclass)
      pv = self$param_set$values
      ctrl = pv[which(!(names(pv) %in% c("measure", "algorithm", "lb", "ub")))]

      or = GenSA::GenSA(par = init_threshold, fn = private$objfun(input),
        lower = rep(pv$lower, nclass), upper = rep(pv$upper, nclass), control = ctrl)
      th = or$par / sum(or$par)
      names(th) = cnames
      return(th)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpPredPostproc", PipeOpPredPostproc)


