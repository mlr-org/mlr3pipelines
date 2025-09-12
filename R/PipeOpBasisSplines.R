#' @title Transforms Columns using Splines Methods by Constructing a Model Matrix
#'
#' @usage NULL
#' @name mlr_pipeops_basissplines
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Transforms Columns using Splines Methods by Constructing a Model Matrix.
#'
#' @section Construction:
#' ```
#' po("basissplines", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"basissplines"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with the selected columns transformed according to the specified Splines Method.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `factor`  :: `character(1)` \cr
#'   "Polynomial" when polynomial splines are applied [`splines::bs`] or
#'   "Cubic" when natural cubic splines are applied [`splines::bs`].
#'   Default is "polynomial".
#' * `df`  :: `integer(1)` \cr
#'   Number of degrees of freedom for calculation of splines basis matrix.
#'   Default is NULL.
#'
#' @section Internals:
#' For creating the Splines uses the [`splines::bs`]/[`splines::ns`] function.
#' Uses the [`stats::model.matrix()`] function.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("basissplines")
#'
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export

PipeOpBasisSplines = R6Class("PipeOpBasisSplines",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "basissplines", param_vals = list()) {
    ps = ps(
      factor = p_fct(levels = c("polynomial", "cubic"), init = "polynomial", tags = c("train", "basissplines", "required")),
      df = p_int(lower = 1, upper = Inf, special_vals = list(NULL), init = NULL, tags = c("train", "basissplines")),
      degree = p_int(lower = 1, upper = Inf, depends = factor == "polynomial", tags = c("train", "basissplines"))
    )
    super$initialize(id = id, param_set = ps, param_vals = param_vals)
    }
  ),
  private = list(
    .transform_dt = function(dt, levels) {
      #browser()
      pv = self$param_set$get_values(tags = "basissplines")
      single_string = mlr3misc::invoke(.f = paste0, .args = list(if (pv$factor == "polynomial") "splines::bs(dt[[" else "splines::ns(dt[[", seq_along(dt), "]]", if (!is.null(pv$df)) " , df = ", pv$df, if (!is.null(pv$degree)) ", degree = ", pv$degree, ")"))
      string = paste(" ~ ", paste(single_string, collapse = " + "))
      result = as.data.frame(stats::model.matrix(formula(string), data = dt))
      max_df = as.numeric(max(regmatches(colnames(result), regexpr("[0-9]+$", colnames(result)))))
      k = 1
      for (j in colnames(dt)) {
        for (i in seq_len(max_df)) {
          colnames(result)[k + 1] = paste0("splines.", j, ".", tail(strsplit(colnames(result)[[k + 1]], "")[[1]], 1))
          k = k + 1
        }
      }
      result
    }
  )
)

mlr_pipeops$add("basissplines", PipeOpBasisSplines)

# po = po("basissplines")
# po_result = po$train(list(tsk("iris")))[[1]]$data()

# po_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length) + splines::bs(Sepal.Width) + splines::bs(Petal.Length) + splines::bs(Petal.Width), data = iris)

# podf3 = po("basissplines", df = 3)
# podf3_result = podf3$train(list(tsk("iris")))[[1]]$data()
# podf3_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length, df = 3) + splines::bs(Sepal.Width, df = 3) + splines::bs(Petal.Length, df = 3) + splines::bs(Petal.Width, df = 3), data = iris)

# podf4 = po("basissplines", df = 4)
# podf4_result = podf4$train(list(tsk("iris")))[[1]]$data()
# podf4_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length, df = 4) + splines::bs(Sepal.Width, df = 4) + splines::bs(Petal.Length, df = 4) + splines::bs(Petal.Width, df = 4), data = iris)

# podf7 = po("basissplines", df = 7)
# podf7_result = podf7$train(list(tsk("iris")))[[1]]$data()
# podf7_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length, df = 7) + splines::bs(Sepal.Width, df = 7) + splines::bs(Petal.Length, df = 7) + splines::bs(Petal.Width, df = 7), data = iris)

# podeg3df2 = po("basissplines", degree = 3, df = 2)
# podeg3df2_result = podeg3df2$train(list(tsk("iris")))[[1]]$data()
# podeg3df2_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length, degree = 3, df = 2) + splines::bs(Sepal.Width, degree = 3, df = 2) + splines::bs(Petal.Length, degree = 3, df = 2) + splines::bs(Petal.Width, degree = 3, df = 2), data = iris)


# podeg5df8 = po("basissplines", degree = 5, df = 8)
# podeg5df8_result = podeg5df8$train(list(tsk("iris")))[[1]]$data()
# podeg5df8_result_calc = stats::model.matrix(Species ~ splines::bs(Sepal.Length, degree = 5, df = 8) + splines::bs(Sepal.Width, degree = 5, df = 8) + splines::bs(Petal.Length, degree = 5, df = 8) + splines::bs(Petal.Width, degree = 5, df = 8), data = iris)

# pons = po("basissplines", factor = "cubic")
# pons_result = pons$train(list(tsk("iris")))[[1]]$data()
# pons_result_calc = stats::model.matrix(Species ~ splines::ns(Sepal.Length) + splines::ns(Sepal.Width) + splines::ns(Petal.Length) + splines::ns(Petal.Width), data = iris)

# pons_error = po("basissplines", factor = "cubic", df = 3, degree = 4)

# ponsdf5 = po("basissplines", factor = "cubic", df = 5)
# ponsdf5_result = ponsdf5$train(list(tsk("iris")))[[1]]$data()
# ponsdf5_result_calc = stats::model.matrix(Species ~ splines::ns(Sepal.Length, df = 5) + splines::ns(Sepal.Width, df = 5) + splines::ns(Petal.Length, df = 5) + splines::ns(Petal.Width, df = 5), data = iris)




# selecting columns
# sel_cyl = selector_grep("cyl|disp|am")
# pos = po("basissplines", affect_columns = sel_cyl)
# pos$train(list(tsk("mtcars")))[[1]]$data()

# pop = po("basissplines", df = 5)
# pop$train(list(tsk("mtcars")))[[1]]$data()

# poc = po("basissplines", df = 4, factor = "cubic")
# poc$train(list(tsk("mtcars")))[[1]]$data()

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
