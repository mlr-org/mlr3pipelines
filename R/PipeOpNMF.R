#' @title Non-negative Matrix Factorization
#'
#' @usage NULL
#' @name mlr_pipeops_nmf
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Extracts non-negative components from data by performing non-negative matrix factorization. Only
#' affects non-negative numerical features. See [`nmf()`][NMF::nmf] for details.
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
#' as well as the elements of the object returned by [`nmf()`][NMF::nmf].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `rank` :: `integer(1)`\cr
#'   Factorization rank, i.e., number of components. Initialized to `2`.
#'   See [`nmf()`][NMF::nmf].
#' * `method` :: `character(1)`\cr
#'   Specification of the NMF algorithm. Initialized to `"brunet"`.
#'   See [`nmf()`][NMF::nmf].
#' * `seed` :: `character(1)` | `integer(1)` | `list()` | object of class `NMF` | `function()`\cr
#'   Specification of the starting point.
#'   See [`nmf()`][NMF::nmf].
#' * `nrun` :: `integer(1)`\cr
#'   Number of runs to performs. Default is `1`.
#'   More than a single run allows for the computation of a consensus matrix which will also be stored in the `$state`.
#'   See [`nmf()`][NMF::nmf].
#' * `debug` :: `logical(1)`\cr
#'   Whether to toggle debug mode. Default is `FALSE`.
#'   See [`nmf()`][NMF::nmf].
#' * `keep.all` :: `logical(1)`\cr
#'   Whether all factorizations are to be saved and returned. Default is `FALSE`.
#'   Only has an effect if `nrun > 1`.
#'   See [`nmf()`][NMF::nmf].
#' * `parallel` :: `character(1)` | `integer(1)` | `logical(1)`\cr
#'   Specification of parallel handling if `nrun > 1`.
#'   Initialized to `FALSE`, as it is recommended to use `mlr3`'s `future`-based parallelization.
#'   See [`nmf()`][NMF::nmf].
#' * `parallel.required` :: `character(1)` | `integer(1)` | `logical(1)`\cr
#'   Same as `parallel`, but an error is thrown if the computation cannot be performed in parallel or
#'   with the specified number of processors.
#'   Initialized to `FALSE`, as it is recommended to use `mlr3`'s `future`-based parallelization.
#'   See [`nmf()`][NMF::nmf].
#' * `shared.memory` :: `logical(1)`\cr
#'   Whether shared memory should be enabled.
#'   See [`nmf()`][NMF::nmf].
#' * `simplifyCB` :: `logical(1)`\cr
#'   Whether callback results should be simplified. Default is `TRUE`.
#'   See [`nmf()`][NMF::nmf].
#' * `track` :: `logical(1)`\cr
#'   Whether error tracking should be enabled. Default is `FALSE`.
#'   See [`nmf()`][NMF::nmf].
#' * `verbose` :: `integer(1)` | `logical(1)`\cr
#'   Specification of verbosity. Default is `FALSE`.
#'   See [`nmf()`][NMF::nmf].
#' * `pbackend` :: `character(1)` | `integer(1)` | `NULL`\cr
#'   Specification of the parallel backend.
#'   It is recommended to use `mlr3`'s `future`-based parallelization.
#'   See [`nmf()`][NMF::nmf].
#' * `callback` | `function()`\cr
#'   Callback function that is called after each run (if `nrun > 1`).
#'   See [`nmf()`][NMF::nmf].
#'
#' @section Internals:
#' Uses the [`nmf()`][NMF::nmf] function as well as [`basis()`][NMF::basis], [`coef()`][NMF::coef] and
#' [`ginv()`][MASS::ginv].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examplesIf mlr3misc::require_namespaces(c("NMF", "MASS"), quietly = TRUE)
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("nmf")
#'
#' task$data()
#' pop$train(list(task))[[1]]$data()
#'
#' pop$state
#' \dontshow{ try(rm("format.list", envir = .BaseNamespaceEnv$.__S3MethodsTable__.), silent = TRUE)  # BiocGenerics overwrites printer for our tables mlr-org/mlr3#1112 }
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpNMF = R6Class("PipeOpNMF",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "nmf", param_vals = list()) {
      ps = ps(
        rank = p_int(lower = 1L, upper = Inf, tags = c("train", "nmf")),
        method = p_fct(tags = c("train", "nmf"),
          levels = c("brunet", "lee", "ls-nmf", "nsNMF", "offset", "pe-nmf", "snmf/r", "snmf/l")),
        seed = p_uty(tags = c("train", "nmf")),
        # NOTE: rng missing, not well documented
        nrun = p_int(lower = 1L, upper = Inf, default = 1L, tags = c("train", "nmf")),
        # NOTE: model missing, probably over the top here
        # the following are .options
        debug = p_lgl(default = FALSE, tags = c("train", "nmf.options")),
        keep.all = p_lgl(default = FALSE, tags = c("train", "nmf.options")),
        parallel = p_uty(default = TRUE, tags = c("train", "nmf.options")),
        parallel.required = p_uty(tags = c("train", "nmf.options")),
        shared.memory = p_lgl(tags = c("train", "nmf.options")),
        simplifyCB = p_lgl(default = TRUE, tags = c("train", "nmf.options")),
        track = p_lgl(default = FALSE, tags = c("train", "nmf.options")),
        verbose = p_uty(default = FALSE, tags = c("train", "nmf.options")),
        pbackend = p_uty(tags = c("train", "nmf")),  # .pbackend
        callback = p_uty(tags = c("train", "nmf"), depends = quote(keep.all == TRUE))  # .callback
      )
      ps$values = list(rank = 2L, method = "brunet", parallel = FALSE, parallel.required = FALSE)
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"), packages = c("MASS", "NMF"))
    }
  ),
  private = list(

    .train_dt = function(dt, levels, target) {
      x = t(as.matrix(dt))  # nmf expects a matrix with the rows holding the features

      # handling of parameters
      .args = self$param_set$get_values(tags = "nmf")
      names(.args)[match("pbackend", names(.args), nomatch = 0L)] = ".pbackend"
      names(.args)[match("callback", names(.args), nomatch = 0L)] = ".callback"

      nmf = mlr3misc::invoke(NMF::nmf,
        x = x,
        rng = NULL,
        model = NULL,
        .args = .args,
        .options = self$param_set$get_values(tags = "nmf.options")
      )

      self$state = nmf
      # here we have two options? return directly h or do what we do during prediction
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
      non_negative = map(task$data(cols = features), function(x) all(x >= 0))  # could also be more precise
      names(non_negative[unlist(non_negative)])
    }
  )
)

mlr_pipeops$add("nmf", PipeOpNMF)

# this is just a really bad idea
## CondLarger = R6Class("CondLarger", inherit = Condition,
##   public = list(
##     initialize = function(rhs) super$initialize("larger", rhs),
##     test = function(x) !is.na(x) & x > self$rhs,
##     as_string = function(lhs_chr = "x") sprintf("%s > %s", lhs_chr, as.character(self$rhs))
##   )
## )
