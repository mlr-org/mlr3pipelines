# DRAFT

PipeOpBasisSplines = R6Class("PipeOpBasisSplines",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "basissplines", param_vals = list()) {
    ps = ps(
      factor = p_fct(levels = c("polynomial", "cubic"), default = "polynomial", tags = c("train", "basissplines")), # tag basissplines?
      df = p_int(lower = 1, upper = Inf, tags = c("train", "basissplines"))
    )
      ps$values = list()
      super$initialize(id = id, param_set = ps, param_vals = param_vals)
    }
  ),
  private = list(
    .transform_dt = function(dt, levels) {
      browser()
      single_string = paste0(
        "splines::ns(mtcars[[", seq_along(dt), "]] , ", df, ")")
      formula = paste(single_string, collapse = " + ")
      stats::model.matrix(as.formula(paste(task$target_names , "~", formula)), data = dt)
      dt
    }
    # output
  )
)

mlr_pipeops$add("basissplines", PipeOpBasisSplines)

po = po("basissplines")

po$train(list(tsk("mtcars")))

# df als hyperparameterf
# das ziel ist es dass wir diese model.matrix für alle features kriegen
# original features behalten dann feature_union ==> egaluser verantwortung


#splines.cyl.1

task = tsk("mtcars")

list(task)[[1]]$data()
pop = po("modelmatrix", formula = ~ splines::ns(task$data()$cyl, 2) + splines::ns(task$data()$hp, 2))
pop$train(list(task))[[1]]$data()

fit <- lm(mpg ~ splines::ns(cyl, df = 2) + splines::ns(hp, df = 2), data = mtcars)
model.matrix(fit) # this is what we want to get as a result from PipeOpSplineBasis

as.data.frame(stats::model.matrix(mpg ~ splines::ns(cyl, 2) + splines::ns(task$data()$hp, 2), data = mtcars))
