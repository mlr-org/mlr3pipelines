#' @include utils.R
#'
#' @title PipeOp
#' @format [R6Class] PipeOp
#'
#' @description
#' A PipeOp is a single tranformation of inputs into outputs.
#' This class is the baseclass for all pipeOp's, and is not intended for direct use.
#' During training it takes inputs, tranforms them, while doing that learns and stores its
#' parameters and then returns the output.
#' During prediction it applies the learned params to the input and transforms the input to an output.'
#'
#' A PipeOp specifies the types of inputs and outputs as `intype` and `outtype`, a list of <something specifying types>.
#' The length of these lists determines the length of input / output the PipeOp produces. The PipeOp input / output
#' is a list of specified length.
#'
#' @section Public Members / Active Bindings
#' * `id`                         :: [character]
#'   Active binding that allows to return and set the id of the PipeOps. Ids are user-configurable, and ids of PipeOps in graphs must be unique.
#' * `packages`                   :: [character]
#'   Packages required for the pipeOp.
#' * `param_set`                  :: [ParamSet]
#'   The set of all exposed parameters of the PipeOp.
#' * `par_vals`                   :: named [list]
#'   Parameter settings where all setting must come from `param_set`, named with param IDs.
#' * `params`                     :: [anys]
#'   The object of learned parameters, obtained in the training step, and applied in the predict step.
#' * `is_trained`                 :: [logical(1)]
#'   Is the PipeOp currently trained?
#' * `params`                     :: any
#' * `result`                     :: any
#'   A slot to store the result of either the `train` or the `predict` step, after it was
#' * `intype`                     :: [list]
#'   Input types the pipeOp accepts. Read-only.
#' * `outtype`                    :: [list]
#'   Output types that are returned by the pipeOp. Read-only.
#'
#' @section Methods
#' * new(id, params)` \cr
#'   `character(1)`, `ParamSet` -> [PipeOp]
#'   Constructs the pipeOp from an id string and a (possibly empty) [ParamSet].
#' * `train()`:
#'    Train graph on `input`, transform it to output and store the learned `params`.
#'    If the PipeOp is already trained, already present `params` are overwritten.
#' * `predict()` \cr
#' *  Predict with graph on `input`, and transform it to output by applying the learned `params`.
#' *   If `is_trained = FALSE` the function cannot be applied.
#' * `print()`: Prints
#'
#' @section Internals:
#' * `.intype`: `list of any`
#' * `.outtype`: `list of any`
#'
#' @name PipeOp
#' @family PipeOp
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
      catf("is_trained=%s", self$is_trained)
      catf("Input: %s", as_short_string(self$inputs))
      catf("Result: %s", as_short_string(self$result))
    },

    train = function(...) stop("no train function given"),  # TODO: some better way to make smth abstract?
    predict = function(...) stop("no predict function given")
  ),

  active = list(
    id = function(id) {  # [character(1)] identifier of PipeOp within the Graph. Always call containing graph's update_ids() when changing this.
      if (missing(id)) {
        private$.id
      } else {
        private$.id = id
        # TODO: maybe notify the graph about changed ID?
      }
    },
    param_set = function() private$.param_set,  # [ParamSet] ParamSet of the PipeOp
    param_vals = function(vals) {  # [named list] parameter values, one for each parameter of the ParamSet. Mutable, with feasibility check.
      if (!missing(vals)) {
        # TODO: param check
        if (!self$param_set$test(vals)) {
          stop("Parameters out of bounds")
        }
        private$.param_vals = vals
      }
      private$.param_vals
    },
    intype = function() private$.intype,  # [list, indexed by channel_id] input types
    outtype = function() private$.outtype,  # [list, indexed by channel_id] output types

    # ------------ BELOW HERE SHOULD BE DROPPED AT SOME POINT
    is_trained = function() !is.null(self$state)  # [logical(1)] whether the `train()` function was called at least once.
  ),

  private = list(
    .id = NULL,
    .param_set = NULL,
    .param_vals = NULL,
    .intype = NULL,
    .outtype = NULL
  )
)
