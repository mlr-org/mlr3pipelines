#' @include utils.R
#'
#' @title PipeOp
#' @format [R6Class] PipeOp
#'
#' @description
#' A `PipeOp` represents a tranformation of "input" into "output", during multiple stages: "training"
#' and "prediction". It can be understood as a generalisation of function with multiple outputs and
#' multiple stages. The "training" stage can be used when training a machine learning pipeline or
#' fitting a statistical model, and the "predicting" stage can then be used when making predictions
#' on new data.
#'
#' During training, a `PipeOp` takes inputs and tranforms them while simultaneously storing a state
#' in the `$state` slot. During prediction, information in the `$state` slot is often used for prediction
#' on the new data.
#'
#' A `PipeOp` is usually used in a [`Graph`] object, a representation of a computational graph. It can have
#' multiple **input channels**---think of these as multiple arguments to a function, for example when averaging
#' different models---, and multiple **output channels**, the "dual" to input channels---a transformation may
#' return different objects, for example different subsets of a [`Task`], that can then be processed by distinct
#' functions.
#'
#' Input and output channels are defined in the `$input` and `$output` slots; each channel has a *name*, a required
#' type during training, and a required type during prediction. When inheriting from a `PipeOp`, the `$train()` and
#' `$predict()` functions must be overloaded. Each of these functions then receives a named `list` according to
#' the `PipeOp`'s input channels, and must return a `list` (names are ignored) with values in the order of output
#' channels in `$output`.
#'
#' The `$train()` and `$predict()` function should not be called by the user; instead, a `PipeOp` should be added
#' to a `Graph` (possibly as singleton in that `Graph`), and the `Graph`'s `$train` / `$predict` methods should be
#' called.
#'
#' This class is an abstract base class that all `PipeOp`s being used in a [`Graph`] should inherit from,  and
#' is not intended to be instantiated.
#'
#' @section Public Members / Active Bindings:
#' * `id`                         :: `character` \cr
#'   ID of the `PipeOp`. IDs are user-configurable, and IDs of `PipeOp`s must be unique within a `Graph`. IDs of
#'   `PipeOp`s must not be changed once they are part of a `Graph`, instead the `Graph`'s `$set_names()` method
#'   should be used.
#' * `packages`                   :: `character` \cr
#'   Set of all required packages for the `PipeOp`'s `$train` and `$predict` methods.
#' * `param_set`                  :: [`ParamSet`] \cr
#'   Parameters and parameter constraints. See `param_set$values`.
#' * `param_set$values`                 :: named `list` \cr
#'   Parameter values that influence the functioning of `$train` and / or `$predict`. Parameter values are checked
#'   against parameter constraints in `$param_set`.
#' * `state`                      :: `any` \cr
#'   Method-dependent state obtained during training step, and usually required for the prediction step.
#' * input                        :: [`data.table`] with `character` columns `name`, `train`, `predict` \cr
#'   Input channels of `PipeOp`. Column `name` gives the names (and order) of values in the list given to
#'   `$train()` and `$predict()`. Column `train` is the (S3) class that an input object must conform to during
#'   training, column `predict` is the (S3) class that an input object must conform to during prediction. Types
#'   are checked by the `PipeOp` and do not need to be checked by `$train` / `$predict` code.
#' * output                       :: [`data.table`] with `character` columns `name`, `train`, `predict` \cr
#'   Output channels of `PipeOp`, in the order in which they must be given in the list returned by `$train` and
#'   `$predict` functions. Column `train` is the (S3) class that an output object must conform to during training,
#'   column `predict` is the (S3) class that an output object must conform to during prediction. The `PipeOp` checks
#'   values returned by `$train` and `$predict` against these types specifications.
#' * `innum`                      :: `numeric(1)` \cr
#'   Number of input channels. This equals `nrow($input)`.
#' * `outnum`                     :: `numeric(1)` \cr
#'   Number of output channels. This equals `nrow($output)`.
#' * `is_trained`                 :: `logical(1)` \cr
#'   Is the PipeOp currently trained and can therefore be used for prediction?
#' * `hash`                       :: `character(1)` \cr
#'   Checksum calculated on the `PipeOp`, depending on the `PipeOp`'s `class` and the slots `$id` and `$param_set`
#'   (and therefore also `$param_set$values`). If a
#'   `PipeOp`'s functionality may change depending on more than these values, it should inherit the `$hash` active
#'   binding and calculate the hash as `digest(list(super$hash, <OTHER THINGS>), algo = "xxhash64")`.
#' * `.result`                    :: `list` \cr
#'   If the `Graph`'s `$keep_results` is set to `TRUE`, then the intermediate Results of `$train()` and `$predict()`
#'   are saved to this slot, exactly as they are returned by these functions. This is mainly for debugging purposes
#'   and done, if requested, by the `Graph` backend itself; it should *not* be done explicitly by `$train()` or `$predict()`.
#'
#' @section Methods:
#' * `PipeOp$new(id, param_set = ParamSet$new(), input, output)` \cr
#'   (`character(1)`, `ParamSet`, [`data.table`], [`data.table`]) -> `PipeOp` \cr
#'   Constructs the pipeOp from an ID string, a (possibly empty) [`ParamSet`], an and an input and output channel
#'   description. `input` and `output` must conform to the `$input` and `$output` slots described above. `PipeOp`s
#'   that inherit from this class and which are not abstract should have an `$initialize()` function with a default value
#'   for `id`, should construct the `param_set`, `input` and `output` values inside that function, and then call `super$initialize`
#'   with these values.
#' * `train(input)` \cr
#'   (named `list`) -> `list` \cr
#'   Train `PipeOp` on `inputs`, transform it to output and store the learned `$state`. If the PipeOp is already
#'   trained, already present `$state` is overwritten. Input list is named according to `$input` `name` column, and was
#'   typechecked against the `$input` `type` column. Return value should be a list with as many entries as `$output` has
#'   rows, with each entry having a class according to the `$output` `train` column. Names of the returned list are ignored.\cr
#'   The `$train()` method should not be called by a user; instead, the `PipeOp` should be added to a `Graph`, and the `Graph`'s
#'   `$train()` method should be used.
#' * `predict(input)` \cr
#'   (named `list`) -> `list` \cr
#'   Predict on new data in `input`, possibly using the stored `$state`. Unlike `$train()`, `$predict()` should not modify
#'   the `PipeOp` in any way. `$predict()` will not be called by a `Graph` if the `$state` is empty, so its presence should
#'   not be checked. Input and output are specified by `$input` and `$output` in the same way as for `$train()`, except that
#'   the `predict` column is used for type checking.\cr
#'   Just as `$train()`, `$predict` should not be called by a user; instead a `Graph`'s `$predict()` method should be used.
#' * `train_internal(input)` \cr
#'   (named `list`) -> `list` \cr
#'   Internal function used by a `Graph` to call `$train()`. Does type checking and possibly skips a function call if a
#'   [`NO_OP`] is received. Should not be overloaded by inheriting classes.
#' * `predict_internal(input)` \cr
#'   (named `list`) -> `list` \cr
#'   Internal function used by a `Graph` to call `$predict()`, function analogous to `$train_internal()`.
#' * `print()` \cr
#'   () -> `NULL` \cr
#'   Prints the `PipeOp`s most salient information: `$id`, `$is_trained`, `$param_set$values`, `$input` and `$output`.
#'
#' @name PipeOp
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @export
PipeOp = R6Class("PipeOp",
  public = list(
    packages = NULL,
    state = NULL,
    input = NULL,
    output = NULL,
    .result = NULL,

    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), input, output, packages = character(0)) {
      private$.param_set = assert_param_set(param_set)
      self$param_set$values = insert_named(self$param_set$values, param_vals)
      self$id = assert_string(id) # also sets the .param_set$set_id
      self$input = assert_connection_table(input)
      self$output = assert_connection_table(output)
      self$packages = assert_character(packages, any.missing = FALSE, unique = TRUE)
      require_namespaces(self$packages)
    },

    print = function(...) {
      type_table_printout = function(table) {
        strings = do.call(sprintf, cbind(fmt = "%s`[%s,%s]", table[, c("name", "train", "predict")]))
        strings = strwrap(paste(strings, collapse = ", "), indent = 2, exdent = 2)
        if (length(strings) > 6) {
          strings = c(strings[1:5], sprintf("  [... (%s lines omitted)]", length(strings) - 5))
        }
        gsub("`", " ", paste(strings, collapse = "\n"))
      }

      catf("PipeOp: <%s> (%strained)", self$id, if (self$is_trained) "" else "not ")
      catf("values: <%s>", as_short_string(self$param_set$values))
      catf("Input channels <name [train type, predict type]>:\n%s", type_table_printout(self$input))
      catf("Output channels <name [train type, predict type]>:\n%s", type_table_printout(self$output))
    },

    train_internal = function(input) {
      if (every(input, is_noop)) {
        self$state = NO_OP
        return(named_list(self$output$name, NO_OP))
      }
      check_types(self, input, "input", "train")
      output = self$train(input)
      check_types(self, output, "output", "train")
      output
    },
    predict_internal = function(input) {
      if (every(input, is_noop)) {
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
    id = function(val) {
      if (!missing(val)) {
        private$.id = val
        private$.param_set$set_id = val
      }
      private$.id
    },
    param_set = function(val) {
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },
    innum = function() nrow(self$input),
    outnum = function() nrow(self$output),
    is_trained = function() !is.null(self$state),
    hash = function() {
      digest(list(class(self), self$id, self$param_set),
        algo = "xxhash64")
    },
    values = function(val) {
      if (!missing(val)) {
        self$param_set$values = val
      }
      self$param_set$values
    }),

  private = list(
    .param_set = NULL,
    .id = NULL
  )
)

assert_connection_table = function(table) {
  varname = deparse(substitute(table))
  assert_data_table(table, .var.name = varname, min.rows = 1)
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
    if (typereq != "*") {
      assert_class(data[[idx]], typereq,
        .var.name = sprintf("%s %s (\"%s\") of PipeOp %s",
          direction, idx, self$input$name[idx], self$id))
    }
  }
}
