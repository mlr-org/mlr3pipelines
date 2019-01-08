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
#' @section Public Members / Active Bindings:
#' * `id`                         :: [character]
#'   Active binding that allows to return and set the id of the PipeOps. Ids are user-configurable, and ids of PipeOps in graphs must be unique.
#' * `packages`                   :: [character]
#'   Packages required for the pipeOp.
#' * `param_set`                  :: [ParamSet]
#'   The set of all exposed parameters of the PipeOp.
#' * `param_vals`                   :: named [list]
#'   Parameter settings where all setting must come from `param_set`, named with param IDs.
#' * `state`                      :: [anys]
#'   The object of learned parameters, obtained in the training step, and applied in the predict step.
#' * `is_trained`                 :: [logical(1)]
#'   Is the PipeOp currently trained?
#' * `result`                     :: any
#'   A slot to store the result of either the `train` or the `predict` step, after it was
#' * `train_intypes`                       :: [character]
#'   Input types the PipeOp accepts during training. Read-only.
#' * `train_outtypes`                      :: [character]
#'   Output types that are returned by the PipeOp during training. Read-only.
#' * `predict_intypes`                     :: [character]
#'   Input types the PipeOp accepts during prediction. Read-only.
#' * `predict_outtypes`                    :: [character]
#'   Output types that are returned by the PipeOp. Read-only.
#'
#' @section Methods:
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
    train_intypes = "any",
    train_outtypes = "any",
    predict_intypes = "any",
    predict_outtypes = "any",

    initialize = function(id, param_set = ParamSet$new(), param_vals = NULL) {
      private$.id = id
      private$.param_set = param_set
      #FIXME: we really need a function in paradox now to get defaults
      private$.param_vals = param_set$defaults
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

    train = function(...) stop("abstract"),
    predict = function(...) stop("abstract")
  ),

  active = list(
    #FIXME: the comment below seems weird. Either we do that automatically or we should not allow changing of ids?
    id = function(id) {  # Always call containing graph's update_ids() when changing this.
      if (missing(id)) {
        private$.id
      } else {
        private$.id = id
        # FIXME: maybe notify the graph about changed ID?
      }
    },
    param_set = function() private$.param_set,
    param_vals = function(vals) {
      if (!missing(vals)) {
        # FIXME: param check
        if (!self$param_set$test(vals)) {
          stop("Parameters out of bounds")
        }
        private$.param_vals = vals
      }
      private$.param_vals
    },
    intype = function() private$.intype,
    outtype = function() private$.outtype,

    # ------------ BELOW HERE SHOULD BE DROPPED AT SOME POINT
    is_trained = function() !is.null(self$state)
  ),

  private = list(
    .id = NULL,
    .param_set = NULL,
    .param_vals = NULL
  )
)
