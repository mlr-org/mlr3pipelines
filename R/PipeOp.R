#' @include utils.R
#' @title PipeOp
#' @format [R6Class] PipeOp
#'
#' @description
#'   A PipeOp is a single tranformation of inputs into outputs.
#'   This class is the baseclass for all pipeOp's, and is not intended for direct use.
#'   During training it takes inputs, tranforms them, while doing that learns and stores its
#'   parameters and then returns the output.
#'   During prediction it applies the learned params to the input and transforms the input to an output.'
#'
#'   A PipeOp specifies the types of inputs and outputs as `intype` and `outtype`, a list of <something specifying types>.
#'   The length of these lists determines the length of input / output the PipeOp produces. Typically the PipeOp input / output
#'   is a list of specified length, but PipeOps with input / output length 1 can specify that they don't use lists but use singular
#'   values instead (`.takeslist` / `.returnslist` set to `FALSE`)#'
#'
#' @section Usage:
#' * `f = pipeOp$new(id, params)` \cr
#'     `character(1)`, `ParamSet` -> [PipeOp]
#' * `f$id` -> `character(1)`
#' * `f$packages -> `character`
#' * `f$param_set` -> `ParamSet`
#' * `f$par_vals` -> `named list`
#' * `f$is_trained` -> `logical(1)`
#' * `f$params` -> `any`
#' * `f$result` -> `any`
#' * `f$intype` -> `list of any`
#' * `f$outtype` -> `list of any`
#' * `f$takeslist` -> `logical(1)`
#' * `f$returnslist` -> `logical(1)`
#' * `f$print()`
#' * `f$train()` \cr
#'   `any` -> `any`
#' * `f$predict()` \cr
#'   `any` -> `any`
#'
#' @section Details:
#' * `new()`: Constructs the pipeOp from an id string and a (possibly empty) [ParamSet].
#' * `id`: Active binding that allows to return and set the id of the PipeOps. Ids are user-configurable, and ids of PipeOps in graphs must be unique.
#' * `packages`: Packages required for the pipeOp.
#' * `param_set`: The set of all exposed parameters of the PipeOp.
#' * `par_vals`: A named list of parameter settings where all setting must come from `param_set`.
#' * `is_trained`: Is the PipeOp currently trained?
#' * `params`: The object of learned parameters, obtained in the training step, and applied in the predict step.
#' * `result`: A slot to store the result of either the `train` or the `predict` step, after it was
#' *   applied.
#' * `print()`: Prints
#' * `train()`: Function that is responsible to train on `input`, transform it to output and store the learned `params`.
#' *   If the PipeOp is already trained, already present `params` are overwritten.
#' * `predict()`: Function that is responsible to predict on `input`, and transform it to output by applying the learned `params`.
#' *   If `is_trained = FALSE` the function cannot be applied.
#' * `intype`: list of input types the pipeOp accepts. Read-only.
#' * `outtype`: list of output types that are returned by the pipeOp. Read-only.
#' * `takeslist`: `TRUE` if input of `train` / `predict` is a list, `FALSE` if it is a singular value.
#'   If this is `FALSE`, `length(.intype)` must be 1.  Read-only.
#' * `returnslist`: `TRUE` if output of `train` / `predict` is a list, `FALSE` if it is a singular value.
#'   If this is `FALSE`, `length(.outtype)` must be 1.  Read-only.
#'
#' @section Internals:
#' * `.intype`: `list of any`
#' * `.outtype`: `list of any`
#' * `.takeslist`: `logical(1)`
#' * `.returnslist`: `logical(1)`'
#'
#' @name pipeOp
#' @family pipeOp
PipeOp = R6::R6Class("PipeOp",
  public = list(
    packages = character(0),
    state = NULL,
    result = NULL,
    initialize = function(id, param_set = ParamSet$new(), param_vals = NULL) {
      private$.id = id
      private$.param_set = param_set
      #FIXME: we really need a function in paradox now to get defaults
      private$.param_vals = param_set$data$default
      names(private$.param_vals) = param_set$ids
      private$.param_vals = insert_named(private$.param_vals, param_vals)
      if (!param_set$test(private$.param_vals)) {
        stop("Parameters out of bounds")
      }
    },

    print = function(...) {
      catf("PipeOp: <%s>", self$id)
      catf("parvals: <%s>", as_short_string(self$param_vals))
      catf("is_learnt=%s", self$is_learnt)
      catf("Input: %s", as_short_string(self$inputs))
      catf("Result: %s", as_short_string(self$result))
    },

    train = function(...) stop("no train function given"),  # TODO: some better way to make smth abstract?
    predict = function(...) stop("no predict function given")
  ),

  active = list(
    id = function(id) {
      if (missing(id)) {
        private$.id
      } else {
        private$.id = id

        # TODO: maybe notify the graph about changed ID?
      }
    },
    param_set = function() private$.param_set,
    param_vals = function(vals) {
      if (!missing(vals)) {
        # TODO: param check
        if (!self$param_set$test(vals)) {
          stop("Parameters out of bounds")
        }
        private$.param_vals = vals
      }
      private$.param_vals
    },
    intype = function() private$.intype,
    outtype = function() private$.outtype,
    takeslist = function() {
      tl = private$.takeslist
      assert(tl || length(self$intype) == 1)
      tl
    },
    returnslist = function() {
      rl = private$.returnslist
      assert(rl || length(self$outtype) == 1)
      rl
    },

    # ------------ BELOW HERE SHOULD BE DROPPED AT SOME POINT
    is_trained = function() !is.null(self$state)
  ),

  private = list(
    .id = NULL,  # id, name within a graph, must be unique within that graph
    .param_set = NULL,
    .param_vals = NULL,
    .intype = NULL,  # list of character vectors, identifying the input classes
    .outtype = NULL,  # list of character vectors, identifying output classes
    .takeslist = TRUE,  # may be FALSE, but only if length(intype) is 1
    .returnslist = TRUE  # may be FALSE, but only if length(outtype) is 1
  )
)
