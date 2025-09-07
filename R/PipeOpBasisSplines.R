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
#' * `hp`  :: `character(1)` \cr
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
      #browser()
    ps = ps(
      factor = p_fct(levels = c("polynomial", "cubic"), init = "polynomial", tags = c("train", "basissplines")), # tag basissplines?
      df = p_int(init = 1, lower = 1, upper = Inf, tags = c("train", "basissplines"))
    )
    super$initialize(id = id, param_set = ps, param_vals = param_vals)
    }
  ),
  private = list(
    .transform_dt = function(dt, levels) {
      #browser()
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
