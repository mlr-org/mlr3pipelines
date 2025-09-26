#' @title Transforms Columns using Splines Methods by Constructing a Model Matrix
#'
#' @usage NULL
#' @name mlr_pipeops_splines
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Transforms Columns using Splines Methods by Constructing a Model Matrix.
#'
#' @section Construction:
#' ```
#' po("splines", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"splines"`.
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
#'   "natural" when natural natural splines are applied [`splines::bs`].
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
#' pop = po("splines")
#'
#' pop$train(list(task))[[1]]$data()
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export

PipeOpSplines = R6Class("PipeOpSplines",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "splines", param_vals = list()) {
    ps = ps(
      type = p_fct(levels = c("polynomial", "natural"), init = "natural", tags = c("train", "required")),
      df = p_int(lower = 1, upper = Inf, special_vals = list(NULL), init = NULL, tags = "train"),
      knots = p_uty(special_vals = list(NULL), init = NULL, tags = "train"),
      degree = p_int(lower = 1, upper = Inf, default = 3, depends = type == "polynomial", tags = "train"),
      intercept = p_lgl(init = FALSE, tags = "train"),
      Boundary.knots = p_uty(init = NULL, tags = "train")
    )
    super$initialize(id = id, param_set = ps, param_vals = param_vals, packages = c("splines", "stats"))
    }
  ),
  private = list(
    .transform_dt = function(dt, levels) {
      browser()
      result = list()
      bk = list()
      pv = self$param_set$get_values(tags = "train")
      if (pv$type == "polynomial") {
      for (i in seq_len(ncol(dt))) {
        args = list(
          x = dt[[i]],
          intercept = pv$intercept,
          warn.outside = FALSE)
        if (!is.null(pv$df)) args$df = pv$df
        if (!is.null(pv$knots)) args$knots = pv$knots
        if (!is.null(pv$degree)) args$degree = pv$degree
        if (!is.null(pv$Boundary.knots)) args$Boundary.knots = pv$Boundary.knots[[i]] else args$Boundary.knots = range(dt[[i]])
        result[[i]] <- as.data.frame(do.call(splines::bs, args))
        colnames(result[[i]]) = paste0("splines.", colnames(dt)[[i]], ".", seq_len(ncol(result[[i]])))
        bk[[i]] = attributes(splines::bs(dt[[i]]))$Boundary.knots
      }
        self$param_set$values$Boundary.knots = bk
      } else {
      for (i in seq_len(ncol(dt))) {
        args = list(
          x = dt[[i]],
          intercept = pv$intercept
        )
        if (!is.null(pv$df)) args$df = pv$df
        if (!is.null(pv$knots)) args$knots = pv$knots
        if (!is.null(pv$Boundary.knots)) args$Boundary.knots = pv$Boundary.knots[[i]] else args$Boundary.knots = range(dt[[i]])
        result[[i]] = as.data.frame(do.call(splines::ns, args))

        colnames(result[[i]]) = paste0("splines.", colnames(dt)[[i]], ".", seq_len(ncol(result[[i]])))
        bk[[i]] = attributes(splines::bs(dt[[i]]))$Boundary.knots
        }
        self$param_set$values$Boundary.knots = bk
        }
      result
    }
  )
)

mlr_pipeops$add("splines", PipeOpSplines)

po = po("splines")
head(po$train(list(tsk("iris")))[[1]]$data())
head(po$predict(list(tsk("iris")))[[1]]$data())


head(po$predict(list(as_task_classif(head(iris), target = "Species")))[[1]]$data())



po = po("splines", type = "polynomial")
po$train(list(tsk("iris")))


po = po("splines", affect_columns = selector_grep(("Sepal.Length|Petal.Length")))

po$train(list(tsk("iris")))[[1]]$data()
po$predict(list(tsk("iris")))[[1]]$data()


# bk = attributes(splines::bs(iris$Sepal.Length))$Boundary.knots
# splines::bs(head(iris)$Sepal.Length, Boundary.knots = bk)
# head(splines::bs(iris$Sepal.Length, Boundary.knots = bk))

# a = lapply(X = seq_len(ncol(iris)), FUN = function(x) {browser(); splines::bs(iris[[x]])})
