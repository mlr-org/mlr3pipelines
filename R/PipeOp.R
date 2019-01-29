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
#' * `state`                      :: any
#'   The object of learned parameters, obtained in the training step, and applied in the predict step.
#' * `is_trained`                 :: `logical(1)`
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
#' * `new(id, params)` \cr
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
#' @export
PipeOp = R6Class("PipeOp",
  public = list(
    id = NULL,
    packages = character(0),
    state = NULL,
    result = NULL,
    input = NULL,
    output = NULL,

    initialize = function(id, param_set = ParamSet$new(), input = NULL, output = NULL) {
      self$id = assert_string(id)
      private$.param_set = param_set
      private$.param_vals = list()
      self$input = assert_connection_table(input)
      self$output = assert_connection_table(output)
    },

    print = function(...) {
      catf("PipeOp: <%s>", self$id)
      # catf("parvals: <%s>", as_short_string(self$param_vals))
      # catf("is_trained=%s", self$is_trained)
      # catf("Input: %s", as_short_string(self$inputs))
      # catf("Result: %s", as_short_string(self$result))
    },

    train_internal = function(input) {
      if (all(map_lgl(input, is_noop))) {
        # FIXME: maybe we want to skip on `any` NO_OP, but that would require special handling in PipeOpUnbranch.
        # Would require same adjustment in predict_internal
        self$state = NO_OP
        return(named_list(self$output$name, NO_OP))
      }
      check_types(self, input, "input", "train")
      output = self$train(input)
      check_types(self, output, "output", "train")
      output
    },
    predict_internal = function(input) {
      if (all(map_lgl(input, is_noop))) {
        return(named_list(self$output$name, NO_OP))
      }
      if (is_noop(self$state)) {
        stopf("Pipeop %s got NO_OP during train but no NO_OP during predict.", self$id)
      }

      check_types(self, input, "input", "predict")
      output = self$predict(input)
      check_types(self, output, "output", "predict")
      output
    },
    train = function(input) stop("abstract"),
    predict = function(input) stop("abstract")
  ),

  active = list(
    param_set = function() private$.param_set,
    param_vals = function(vals) {
      if (!missing(vals)) {
        if (!self$param_set$test(vals)) {
          stop("Parameters out of bounds")
        }
        private$.param_vals = vals
      }
      private$.param_vals
    },
    innum = function() nrow(self$input),
    outnum = function() nrow(self$output),
    is_trained = function() !is.null(self$state),
    hash = function() {
      digest(list(self$param_set, self$param_vals),
        algo = "xxhash64")
    }
  ),

  private = list(
    .param_set = NULL,
    .param_vals = NULL
  )
)

assert_connection_table = function(table) {
  if (is.null(table))
    return(data.table(name = character(0L), train = character(0L), predict = character(0L)))
  varname = deparse(substitute(table))
  assert_data_table(table, .var.name = varname)
  assert_names(names(table), permutation.of = c("name", "train", "predict"), .var.name = varname)
  assert_character(table$name, any.missing = FALSE, unique = TRUE, .var.name = paste0("'name' column in ", varname))
  table
}


# Checks that data conforms to the type specifications given.
# @param `data` [list of any] is either the input or output given to a train/predict function. it is checked to be a *list* first
#   and then to have the types as given by the `$input` or `$output` data.table.
# @param direction [character(1)] is either `"input"` or `"output"`
# @param operation [character(1)] is either `"train"` or `"predict"`.
check_types = function(self, data, direction, operation) {
  typetable = self[[direction]]
  assert_list(data, len = nrow(typetable))
  for (idx in seq_along(data)) {
    typereq = typetable[[operation]][idx]
    if (typereq == "*")
      next
    assert_class(data[[idx]], typereq,
      .var.name = sprintf("%s %s (\"%s\") of PipeOp %s",
        direction, idx, self$input$name[idx], self$id))
  }
}
