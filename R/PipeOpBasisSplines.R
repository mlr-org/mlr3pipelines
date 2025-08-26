# DRAFT

PipeOpBasisSplines = R6Class("PipeOpBasisSplines",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "basissplines", param_vals = list()) {
    ps = ps(
      factor = p_fct(levels = c("polynomial", "natural cubic"), default = `placeholder`, tags = "train") # tag basissplines?
    )
      ps$values = list()
      super$initialize(id = id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(
    .train_dt = function(dt, levels, target) {
    },
    .predict_dt = function(dt, levels, target) {
    }
  )
)





task = tsk("mtcars")

list(task)[[1]]$data()
pop = po("modelmatrix", formula = ~ splines::ns(task$data()$cyl, 2) + splines::ns(task$data()$hp, 2))
pop$train(list(task))[[1]]$data()

fit <- lm(mpg ~ splines::ns(cyl, df = 2) + splines::ns(hp, df = 2), data = mtcars)
model.matrix(fit) # this is what we want to get as a result from PipeOpSplineBasis

as.data.frame(stats::model.matrix(mpg ~ splines::ns(cyl, 2) + splines::ns(task$data()$hp, 2), data = mtcars))
?model.matrix
