# DRAFT

PipeOpBasisSplines = R6Class("PipeOpBasisSplines",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "basissplines", param_vals = list()) {
      #browser()
    ps = ps(
      factor = p_fct(levels = c("polynomial", "cubic"), init = "polynomial", tags = c("train", "basissplines")), # tag basissplines?
      df = p_int(init = 2, lower = 1, upper = Inf, tags = c("train", "basissplines"))
    )
    super$initialize(id = id, param_set = ps, param_vals = param_vals)
    }
  ),
  private = list(
    .transform_dt = function(dt, levels) {
      browser()
      pv = self$param_set$get_values(tags = "train")
      if (pv$factor == "polynomial") {
        single_string =
          invoke(.f = paste0, .args = list("splines::bs(dt[[", seq_along(dt), "]] , ", pv$df, ")"))
      }
      else {
      single_string =
        invoke(.f = paste0, .args = list("splines::ns(dt[[", seq_along(dt), "]] , ", pv$df, ")"))
      }
      string = paste(" ~ ", paste(single_string, collapse = " + "))
      result = as.data.frame(stats::model.matrix(formula(string), data = dt))
      k = 1
      for (j in colnames(dt)) {
        for (i in seq_len(pv$df)) {
          colnames(result)[k + 1] = paste0("splines.", j, ".", i)
          k = k + 1
        }
      }
      result
    }
  )
)

mlr_pipeops$add("basissplines", PipeOpBasisSplines)

# po = po("basissplines", df = 3)
# sel_cyl = selector_grep("cyl|disp|am")
# po$train(list(tsk("mtcars")))[[1]]$data()

# df als hyperparameterf
# das ziel ist es dass wir diese model.matrix für alle features kriegen
# original features behalten dann feature_union ==> egaluser verantwortung


#splines.cyl.1

# task = tsk("mtcars")

# list(task)[[1]]$data()
# pop = po("modelmatrix", formula = ~ splines::ns(task$data()$cyl, 2) + splines::ns(task$data()$hp, 2) +
#           splines::ns(task$data()$disp, 2) + splines::ns(task$data()$drat, 2) + splines::ns(task$data()$wt, 2) +
#           splines::ns(task$data()$qsec, 2) + splines::ns(task$data()$vs, 2) + splines::ns(task$data()$am, 2) +
#           splines::ns(task$data()$gear, 2) + splines::ns(task$data()$carb, 2))
# pop$train(list(task))[[1]]$data()

# pob = po("modelmatrix", formula = ~ splines::bs(task$data()$cyl, 2) + splines::bs(task$data()$hp, 2) +
#           splines::bs(task$data()$disp, 2) + splines::bs(task$data()$drat, 2) + splines::bs(task$data()$wt, 2) +
#           splines::bs(task$data()$qsec, 2) + splines::bs(task$data()$vs, 2) + splines::bs(task$data()$am, 2) +
#           splines::bs(task$data()$gear, 2) + splines::bs(task$data()$carb, 2))

# pob$train(list(task))[[1]]$data()


# fit <- lm(mpg ~ splines::ns(cyl, df = 2) + splines::ns(hp, df = 2), data = mtcars)
# model.matrix(fit) # this is what we want to get as a result from PipeOpSplineBasis

# as.data.frame(stats::model.matrix(mpg ~ splines::ns(cyl, 2) + splines::ns(task$data()$hp, 2), data = mtcars))
