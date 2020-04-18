#' @title PipeOpNMF
#'
#' @usage NULL
#' @name mlr_pipeops_nmf
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Extracts non-negative components from data by performing non-negative matrix factorization. Only
#' affects non-negative numerical features. See [NMF::nmf()] for details. Note that [NMF::nmf()]
#' sometimes requires to explicitly load the package via `library(NMF)`, i.e., when running multiple
#' runs in parallel, so that all dependencies can be loaded correctly.
#'
#' @section Construction:
#' ```
#' PipeOpNMF$new(id = "nmf", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"nmf"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected numeric features replaced by their
#' non-negative components.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`],
#' as well as the elements of the class returned by [`nmf()`][NMF::nmf].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `rank` :: `integer(1)`\cr
#'   Factorization rank, i.e., number of components. Default is `2`. See [`nmf()`][NMF::nmf].
#' * `method` :: `character(1)`\cr
#'   Specification of the NMF algorithm. Default is `brunet`. See [`nmf()`][NMF::nmf].
#' * `nrun` :: `integer(1)`\cr
#'   Number of runs to performs. More than a single run allows for the computation of a consensus
#'   matrix which will also be stored in the `$state`. See [`nmf()`][NMF::nmf].
#' * `seed` :: `numeric(1)`\cr
#'   Specification of the starting point. Default is `NULL`. See [`nmf()`][NMF::nmf].
#' * `options` :: `list()`\cr
#'   Optional named list of additional parameters passed to [`nmf()`][NMF::nmf].
#'
#' @section Internals:
#' Uses the [`nmf`][NMF::nmf] function as well as [`basis`][NMF::basis], [`coef`][NMF::coef] and
#' [`ginv`][MASS::ginv].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("nmf")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpNMF = R6Class("PipeOpNMF",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "nmf", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamInt$new("rank", lower = 1L, upper = Inf, default = 2L, tags = c("train", "nmf")),
        ParamFct$new("method", default = "brunet", tags = c("train", "nmf"),
          levels = c("brunet", "lee", "ls-nmf", "nsNMF", "offset", "pe-nmf", "snmf/r", "snmf/l")),
        ParamInt$new("nrun", lower = 1L, upper = Inf, default = 1L, tags = c("train", "nmf")),
        ParamDbl$new("seed", lower = -Inf, upper = Inf, special_vals = list(NULL), default = NULL, tags = c("train", "nmf")),
        # FIXME: pass all other options here? Name check them against the formalArgs of nmf?
        ParamUty$new("options", default = NULL, tags = c("train", "nmf"), custom_check = function(x) check_list(x, null.ok = TRUE))
      ))
      ps$values = list(rank = 2L)
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"), packages = c("MASS", "NMF"))
    }
  ),
  private = list(

    .train_dt = function(dt, levels, target) {
      x = t(as.matrix(dt))  # nmf expects a matrix with the rows holding the features
      # handling of additional options
      args = self$param_set$get_values(tags = "nmf")
      if (!is.null(args$options)) {
        args = append(args, args$options)
        args$options = NULL
      }
      nmf = mlr3misc::invoke(NMF::nmf, x = x, .args = args)
      self$state = nmf
      # FIXME: here we have two options? return directly h or do what we do during prediction
      #h = t(mlr3misc::invoke(NMF::coef, object = nmf))
      w = mlr3misc::invoke(NMF::basis, object = nmf)
      h_ = t(mlr3misc::invoke(MASS::ginv, X = w) %*% x)
      colnames(h_) = paste0("NMF", seq_len(self$param_set$values$rank))
      h_
    },

    .predict_dt = function(dt, levels) {
      x = t(as.matrix(dt))
      w = mlr3misc::invoke(NMF::basis, object = self$state)
      h_ = t(mlr3misc::invoke(MASS::ginv, X = w) %*% x)
      colnames(h_) = paste0("NMF", seq_len(self$param_set$values$rank))
      h_
    },

    .select_cols = function(task) {
      # only use non-negative numerical features
      features = task$feature_types[get("type") %in% self$feature_types, get("id")]
      non_negative = map(task$data(cols = features), function(x) all(x >= 0))
      names(non_negative[unlist(non_negative)])
    }
  )
)

mlr_pipeops$add("nmf", PipeOpNMF)
