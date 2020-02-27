#' @export
Optimizer = R6Class("Optimizer",
  public = list(
    measure = NULL,
    packages = NULL,
    initialize = function(param_vals = list(), param_set = ParamSet$new(), packages = character(0)) {
      self$packages = assert_character(packages, any.missing = FALSE, unique = TRUE)
      if (inherits(param_set, "ParamSet")) {
        private$.param_set = assert_param_set(param_set)
      } else {
        lapply(param_set, function(x) assert_param_set(eval(x)))
      }
      self$param_set$values = insert_named(self$param_set$values, param_vals)
    },
    optimize = function(objfun, init_weights = NULL, ...) {
      private$.optimize(objfun, init_weights, ...)
    }
  ),
  active = list(
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        sourcelist = lapply(private$.param_set_source, function(x) eval(x))
        if (length(sourcelist) > 1) {
          private$.param_set = ParamSetCollection$new(sourcelist)
        } else {
          private$.param_set = sourcelist[[1]]
        }
        if (!is.null(self$id)) {
          private$.param_set$set_id = self$id
        }
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),
  private = list(
    .param_set = NULL,
    .optimize = function() stop("Abstract")
  )
)

#' @export
OptimizerNloptr = R6Class("OptimizerNloptr",
  inherit = Optimizer,
  public = list(
    initialize = function(param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamFct$new("algorithm", default = "NLOPT_LN_COBYLA",
          levels = strsplit(nloptr::nloptr.get.default.options()[1, "possible_values"], ", ")[[1]]),
        ParamUty$new("eval_g_ineq", default = function(x) max(x) - 1),
        ParamDbl$new("xtol_rel", default = 10^-4, lower = 0, upper = Inf),
        ParamDbl$new("xtol_abs", default = 0, lower = 0, upper = Inf),
        ParamDbl$new("ftol_rel", default = 0, lower = 0, upper = Inf),
        ParamDbl$new("ftol_abs", default = 0, lower = 0, upper = Inf),
        ParamDbl$new("stopval", default = -Inf, lower = -Inf, upper = Inf),
        ParamInt$new("maxeval", default = 100, lower = 1L, upper = Inf),
        ParamInt$new("maxtime", default = -1L, lower = 0L, upper = Inf, special_vals = list(-1L)),
        ParamDbl$new("lb", default = 0, lower = -Inf, upper = Inf),
        ParamDbl$new("ub", default = 1, lower = -Inf, upper = Inf)
      ))
      ps$values = list(algorithm = "NLOPT_LN_BOBYQA", xtol_rel = 10^-8, lb = 0, ub = 1)
      super$initialize(param_vals = param_vals, param_set = ps, packages = "nloptr")
    }
  ),
  private = list(
    .optimize = function(objfun, init_weights, ...) {
      requireNamespace("nloptr")
      pv = self$param_set$values
      opts = pv[which(names(pv) %nin% c("measure", "eval_g_ineq", "lb", "ub"))]
      opt = nloptr::nloptr(
        x0 = init_weights,
        eval_f = objfun,
        lb = rep(pv$lb, length(init_weights)),
        ub = rep(pv$ub, length(init_weights)),
        eval_g_ineq = pv$eval_g_ineq,
        opts = opts, ...
      )
      opt$solution
    }
  )
)

#' @export
OptimizerGenSA = R6Class("OptimizerGenSA",
  inherit = Optimizer,
  public = list(
    initialize = function(param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamInt$new("maxit", default = 10000L, lower = 1L, upper = Inf),
        ParamDbl$new("threshold.stop", default = NULL, lower = -Inf, upper = Inf, special_vals = list(NULL)),
        ParamInt$new("nb.stop.improvement", default = NULL, lower = 0L, upper = Inf, special_vals = list(NULL)),
        ParamLgl$new("smooth", default = FALSE),
        ParamInt$new("max.call", default = 3000L, lower = 1L, upper = Inf),
        ParamDbl$new("max.time", default = NULL, lower = 1L, upper = Inf, special_vals = list(NULL)),
        ParamInt$new("temperature", default = 250L, lower = 1L, upper = Inf),
        ParamDbl$new("visiting.param", default = 2.5, lower = 0L, upper = Inf),
        ParamDbl$new("acceptance.param", default = -15, lower = -Inf, upper = Inf),
        ParamLgl$new("verbose", default = FALSE),
        ParamLgl$new("simple.function", default = TRUE),
        ParamDbl$new("lower", default = 0, lower = -Inf, upper = Inf),
        ParamDbl$new("upper", default = 1, lower = -Inf, upper = Inf)
      ))
      ps$values = list(smooth = FALSE, max.call = 300L, temperature = 250, visiting.param = 2.5,
        acceptance.param = -15, simple.function = TRUE, lower = 0, upper = 1)
      super$initialize(param_vals = param_vals, param_set = ps, packages = "GenSA")
    }
  ),
  private = list(
    .optimize = function(objfun, init_weights, ...) {
      requireNamespace("GenSA")
      pv = self$param_set$values
      ctrl = pv[which(!(names(pv) %in% c("lower", "upper")))]
      lower = rep(pv$lower, length(init_weights))
      upper = rep(pv$upper, length(init_weights))
      or = GenSA::GenSA(par = init_weights, fn = objfun, lower = lower, upper = upper, control = ctrl, ...)
      or$par
    }
  )
)

check_optimizer = function(x) {
  inherits(x, "Optimizer")
}
