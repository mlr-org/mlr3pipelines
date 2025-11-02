#' @title Transforms Numeric Features into Spline Basis Expansions
#'
#' @usage NULL
#' @name mlr_pipeops_splines
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Replaces numeric features with columns representing spline basis expansions.
#'
#' Depending on the type parameter, constructs polynomial B-splines [`splines::bs()`] or natural cubic splines [`splines::ns()`] for the respective column.
#'
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
#' After training the `Boundary.knots`will be given to the `$state`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `factor`  :: `character(1)` \cr
#'   "Polynomial" when polynomial splines are applied [`splines::bs`] or
#'   "natural" when natural natural splines are applied [`splines::ns`].
#'   Default is "polynomial".
#' * `df`  :: `integer(1)` \cr
#'   Number of degrees of freedom for calculation of splines basis matrix.
#'   Default is NULL.
#'   For further information look up [`splines::bs()`] or [`splines::ns()`].
#' * `knots` :: `list` \cr
#'   The internal breakpoints that define the spline. The default is no knots.
#'   For further information consult [`splines::bs()`] or [`splines::ns()`].
#' * `intercept` :: `logical(1)` \cr
#'   If `TRUE`, an intercept is included in the basis. Default is `FALSE`.
#'   For further information look up [`splines::bs()`] or [`splines::ns()`].
#' * `degree` :: `integer(1)` \cr
#'   This parameter depends on type = "polynomial". Degree of the polynomial used to compute B-splines.
#'   Default is 3. For further information look up [`splines::bs()`].
#' * `Boundary.knots` :: `list` \cr
#'   Boundary points at which to anchor the spline basis. Parameter has to be passed as a named list.
#'   For further information look up [`splines::bs()`] or [`splines::ns()`].
#'
#' @section Internals:
#' For creating the Splines uses the [`splines::bs`]/[`splines::ns`] function.
#' After training, the `Boundary.Knots` that are either defined in the Parameter Set
#' or have been calculated during training will be passed to the `$state` of the PipeOp.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("splines")
#'
#' pop$train(list(task))[[1]]$data()
#'
#' pobk = po("splines", param_vals = list(Boundary.knots = list("Petal.Length" = c(0, 4), "Petal.Width" = c(4, 7), "Sepal.Length" = c(1, 5), "Sepal.Width" = c(3, 6))))
#' pobk$train(list(task))[[1]]$data()
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export

PipeOpSplines = R6Class("PipeOpSplines",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "splines", param_vals = list()) {
      ps = ps(
        type = p_fct(levels = c("polynomial", "natural"), init = "natural", tags = c("train", "splines", "required")),
        df = p_int(lower = 1, upper = Inf, special_vals = list(NULL), init = NULL, tags = c("train", "splines")),
        knots = p_uty(special_vals = list(NULL), init = NULL, tags = c("train", "splines")),
        degree = p_int(lower = 1, upper = Inf, default = 3, depends = type == "polynomial", tags = c("train", "splines")),
        intercept = p_lgl(init = FALSE, tags = c("train", "splines")),
        Boundary.knots = p_uty(tags = c("train", "splines"))
      )
      super$initialize(id = id, param_set = ps, param_vals = param_vals, packages = c("splines", "stats"))
    }
  ),
  private = list(
    .train_dt = function(dt, levels, target) {
      result = list()
      bk = list()
      pv = self$param_set$get_values(tags = "splines")
        for (i in colnames(dt)) {
          args = pv
          args$type = NULL
          args$knots = pv$knots[[i]]
          args$Boundary.knots = pv$Boundary.knots[[i]]
          if (pv$type == "polynomial") {
            result[[i]] = invoke(splines::bs, .args = args, x = dt[[i]], warn.outside = FALSE)
          } else {
            result[[i]] = invoke(splines::ns, .args = args, x = dt[[i]])
          }
          colnames(result[[i]]) = paste0("splines.", seq_len(ncol(result[[i]])))
          bk[[i]] = attributes(result[[i]])$Boundary.knots
        }
        self$state$Boundary.knots = bk
      result
    },
    .predict_dt = function(dt, levels) {
      result = list()
      pv = self$param_set$get_values(tags = "splines")
      for (i in colnames(dt)) {
        args = pv
        args$type = NULL
        args$knots = pv$knots[[i]]
        args$Boundary.knots = self$state$Boundary.knots[[i]]
        if (pv$type == "polynomial") {
          result[[i]] = invoke(splines::bs, .args = args, x = dt[[i]], warn.outside = FALSE)
        } else {
          result[[i]] = invoke(splines::ns, .args = args, x = dt[[i]])
        }
        colnames(result[[i]]) = paste0("splines.", seq_len(ncol(result[[i]])))
      }
      result
    }
  )
)

mlr_pipeops$add("splines", PipeOpSplines)
