#' @title Transforms Numeric Features into Spline Basis Expansions
#'
#' @usage NULL
#' @name mlr_pipeops_splines
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Replaces numeric features with columns representing spline basis expansions.
#'
#' Depending on the type parameter, constructs polynomial B-splines [`splines::bs()`] or natural cubic splines [`splines::ns()`] for the respective column.
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
#' After training the `Boundary.knots` will be given to the `$state`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `type` :: `character(1)` \cr
#'   Controls the type of splines that are to be created. Can be either `polynomial` ([`splines::bs`]) 
#'   or `natural` ([`splines::ns`]). Initializied to `natural`.
#' * `df`  :: `integer(1)` \cr
#'   Number of degrees of freedom for calculation of the spline basis matrix. Initialized to `NULL`.
#'   Depending on `type`, see either [`splines::bs()`] or [`splines::ns()`].
#' * `knots` :: named `list` \cr
#'   Internal breakpoints that define the spline, given as a named list of numeric vectors,
#'   where each name corresponds to a feature and its value specifies the knots for that feature.
#'   Initialized to `NULL`. Depending on `type`, see either [`splines::bs()`] or [`splines::ns()`].
#' * `intercept` :: `logical(1)` \cr
#'   If `TRUE`, an intercept is included in the basis. Default is `FALSE`.
#'   Depending on `type`, see either [`splines::bs()`] or [`splines::ns()`].
#' * `degree` :: `integer(1)` \cr
#'   Degree of the polynomial used to compute polynomial splines. Only used if `type` is `"polynomial"`.
#'   Default is `3`. See [`splines::bs()`].
#' * `Boundary.knots` :: named `list` \cr
#'   Boundary points at which to anchor the spline basis, given as a named list of numeric vectors,
#'   where each name corresponds to a feature and its value specifies the boundary points for that feature.
#'   Initialized to `NULL`. Depending on `type`, see either [`splines::bs()`] or [`splines::ns()`].
#'
#' @section Internals:
#' Creates a spline basis using either [`splines::bs`] or [`splines::ns`] depending on the hyperparameter `type`.
#' After training, the `Boundary.knots` that were either provided by the user or calculated during training are
#' stored in the `PipeOp`'s `$state`.
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
#' pobk = po("splines", Boundary.knots = list(
#'   Petal.Length = c(0, 4), Petal.Width = c(4, 7), Sepal.Length = c(1, 5), Sepal.Width = c(3, 6))
#' )
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
        df = p_int(lower = 1, upper = Inf, special_vals = list(NULL), default = NULL, tags = c("train", "splines")),
        knots = p_uty(special_vals = list(NULL), init = NULL, custom_check = function(x) check_list(x, any.missing = FALSE, null.ok = TRUE, names = "named"),
          tags = c("train", "splines")),
        degree = p_int(lower = 1, upper = Inf, default = 3, depends = quote(type == "polynomial"), tags = c("train", "splines")),
        intercept = p_lgl(default = FALSE, tags = c("train", "splines")),
        Boundary.knots = p_uty(special_vals = list(NULL), init = NULL, custom_check = function(x) check_list(x, any.missing = FALSE, null.ok = TRUE, names = "named"), 
          tags = c("train", "splines"), )
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
