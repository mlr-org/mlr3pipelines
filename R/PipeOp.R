#' @include utils.R
#' @include mlr_pipeops.R
#'
#' @title PipeOp
#'
#' @usage NULL
#' @format Abstract [`R6Class`].
#'
#' @description
#' A [`PipeOp`] represents a transformation of a given "input" into a given "output", with two stages: "training"
#' and "prediction". It can be understood as a generalized function that not only has multiple inputs, but
#' also multiple outputs (as well as two stages). The "training" stage is used when training a machine learning pipeline or
#' fitting a statistical model, and the "predicting" stage is then used for making predictions
#' on new data.
#'
#' To perform training, the `$train()` function is called which takes inputs and transforms them, while simultaneously storing information
#' in its `$state` slot. For prediction, the `$predict()` function is called, where the `$state` information can be used to influence the transformation
#' of the new data.
#'
#' A [`PipeOp`] is usually used in a [`Graph`] object, a representation of a computational graph. It can have
#' multiple **input channels**---think of these as multiple arguments to a function, for example when averaging
#' different models---, and multiple **output channels**---a transformation may
#' return different objects, for example different subsets of a [`Task`][mlr3::Task]. The purpose of the [`Graph`] is to
#' connect different outputs of some [`PipeOp`]s to inputs of other [`PipeOp`]s.
#'
#' Input and output channel information of a [`PipeOp`] is defined in the `$input` and `$output` slots; each channel has a *name*, a required
#' type during training, and a required type during prediction. The `$train()` and `$predict()` function are called with a `list` argument
#' that has one entry for each declared channel (with one exception, see next paragraph). The `list` is automatically type-checked
#' for each channel against `$input` and then passed on to the `$train_internal()` or `$predict_internal()` functions. There the data is processed and
#' a result `list` is created. This `list` is again type-checked for declared output types of each channel. The length and types of the result
#' `list` is as declared in `$output`.
#'
#' A special input channel name is `"..."`, which creates a *vararg* channel that takes arbitrarily many arguments, all of the same type. If the `$input`
#' table contains an `"..."`-entry, then the input given to `$train()` and `$predict()` may be longer than the number of declared input channels.
#'
#' This class is an abstract base class that all [`PipeOp`]s being used in a [`Graph`] should inherit from,  and
#' is not intended to be instantiated.
#'
#' @section Construction:
#' ```
#' PipeOp$new(id, param_set = ParamSet$new(), param_vals = list(), input, output, packages = character(0))
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot.
#' * `param_set` :: [`ParamSet`][paradox::ParamSet] | `list` of `expression`\cr
#'   Parameter space description. This should be created by the subclass and given to `super$initialize()`.
#'   If this is a [`ParamSet`][paradox::ParamSet], it is used as the [`PipeOp`]'s [`ParamSet`][paradox::ParamSet]
#'   directly. Otherwise it must be a `list` of expressions e.g. created by `alist()` that evaluate to [`ParamSet`][paradox::ParamSet]s.
#'   These [`ParamSet`][paradox::ParamSet] are combined using a [`ParamSetCollection`][paradox::ParamSetCollection].
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings given in `param_set`. The
#'   subclass should have its own `param_vals` parameter and pass it on to `super$initialize()`. Default `list()`.
#' * input :: [`data.table`] with columns `name` (`character`), `train` (`character`), `predict` (`character`)\cr
#'   Sets the `$input` slot of the resulting object; see description there.
#' * output :: [`data.table`] with columns `name` (`character`), `train` (`character`), `predict` (`character`)\cr
#'   Sets the `$output` slot of the resulting object; see description there.
#' * packages :: `character`\cr
#'   Set of all required packages for the [`PipeOp`]'s `$train` and `$predict` methods. See `$packages` slot.
#'   Default is `character(0)`.
#'
#' @section Internals:
#' [`PipeOp`] is an abstract class with abstract functions `$train_internal()` and `$predict_internal()`. To create a functional
#' [`PipeOp`] class, these two methods must be implemented. Each of these functions receives a named `list` according to
#' the [`PipeOp`]'s input channels, and must return a `list` (names are ignored) with values in the order of output
#' channels in `$output`. The `$train_internal()` and `$predict_internal()` function should not be called by the user;
#' instead, a `$train()` and `$predict()` should be used. The most convenient usage is to add the [`PipeOp`]
#' to a `Graph` (possibly as singleton in that `Graph`), and using the `Graph`'s `$train()` / `$predict()` methods.
#'
#' @section Fields:
#' * `id` :: `character`\cr
#'   ID of the [`PipeOp`]. IDs are user-configurable, and IDs of [`PipeOp`]s must be unique within a [`Graph`]. IDs of
#'   [`PipeOp`]s must not be changed once they are part of a [`Graph`], instead the [`Graph`]'s `$set_names()` method
#'   should be used.
#' * `packages` :: `character`\cr
#'   Packages required for the [`PipeOp`]. Functions that are not in base R should still be called using `::`
#'   (or explicitly attached using `require()`) in `$train_internal()` *and* `$predict_internal()`, but
#'   packages declared here are checked before any (possibly expensive) processing has started within a [`Graph`].
#' * `param_set` :: [`ParamSet`][paradox::ParamSet]\cr
#'   Parameters and parameter constraints. Parameter values that influence the functioning of `$train` and / or `$predict` are
#'   in the `$param_set$values` slot; these are automatically checked against parameter constraints in `$param_set`.
#' * `state` :: `any` | `NULL`\cr
#'   Method-dependent state obtained during training step, and usually required for the prediction step. This is `NULL`
#'   if and only if the [`PipeOp`] has not been trained. The `$state` is the *only* slot that can be reliably modified during
#'   `$train()`, because `$train_internal()` may theoretically be executed in a different `R`-session (e.g. for parallelization).
#' * input :: [`data.table`] with columns `name` (`character`), `train` (`character`), `predict` (`character`)\cr
#'   Input channels of [`PipeOp`]. Column `name` gives the names (and order) of values in the list given to
#'   `$train()` and `$predict()`. Column `train` is the (S3) class that an input object must conform to during
#'   training, column `predict` is the (S3) class that an input object must conform to during prediction. Types
#'   are checked by the [`PipeOp`] itself and do not need to be checked by `$train_internal()` / `$predict_internal()` code.\cr
#'   A special name is `"..."`, which creates a *vararg* input channel that accepts a variable number of inputs.
#' * output :: [`data.table`] with columns `name` (`character`), `train` (`character`), `predict` (`character`)\cr
#'   Output channels of [`PipeOp`], in the order in which they will be given in the list returned by `$train` and
#'   `$predict` functions. Column `train` is the (S3) class that an output object must conform to during training,
#'   column `predict` is the (S3) class that an output object must conform to during prediction. The [`PipeOp`] checks
#'   values returned by `$train_internal()` and `$predict_internal()` against these types specifications.
#' * `innum` :: `numeric(1)` \cr
#'   Number of input channels. This equals `nrow($input)`.
#' * `outnum` :: `numeric(1)` \cr
#'   Number of output channels. This equals `nrow($output)`.
#' * `is_trained` :: `logical(1)` \cr
#'   Indicate whether the [`PipeOp`] was already trained and can therefore be used for prediction.
#' * `hash` :: `character(1)` \cr
#'   Checksum calculated on the [`PipeOp`], depending on the [`PipeOp`]'s `class` and the slots `$id` and `$param_set`
#'   (and therefore also `$param_set$values`). If a
#'   [`PipeOp`]'s functionality may change depending on more than these values, it should inherit the `$hash` active
#'   binding and calculate the hash as `digest(list(super$hash, <OTHER THINGS>), algo = "xxhash64")`.
#' * `.result` :: `list` \cr
#'   If the [`Graph`]'s `$keep_results` flag is set to `TRUE`, then the intermediate Results of `$train()` and `$predict()`
#'   are saved to this slot, exactly as they are returned by these functions. This is mainly for debugging purposes
#'   and done, if requested, by the [`Graph`] backend itself; it should *not* be done explicitly by `$train_internal()` or `$predict_internal()`.
#'
#' @section Methods:
#' * `train(input)`\cr
#'   (`list`) -> named `list`\cr
#'   Train [`PipeOp`] on `inputs`, transform it to output and store the learned `$state`. If the PipeOp is already
#'   trained, already present `$state` is overwritten. Input list is typechecked against the `$input` `train` column.
#'   Return value is a list with as many entries as `$output` has
#'   rows, with each entry named after the `$output` `name` column and class according to the `$output` `train` column.
#' * `train_internal(input)`\cr
#'   (named `list`) -> `list`\cr
#'   Abstract function that must be implemented by concrete subclasses. `$train_internal()` is called by `$train()` after
#'   typechecking. It must change the `$state` value to something non-`NULL` and return a list of transformed data according to
#'   the `$output` `train` column. Names of the returned list are ignored.\cr
#'   The `$train_internal()` method should not be called by a user; instead, the `$train()` method should be used which does some
#'   checking and possibly type conversion.
#' * `predict(input)` \cr
#'   (`list`) -> named `list`\cr
#'   Predict on new data in `input`, possibly using the stored `$state`. Input and output are specified by `$input` and `$output`
#'   in the same way as for `$train()`, except that
#'   the `predict` column is used for type checking.
#' * `predict_internal(input)`\cr
#'   (named `list`) -> `list`\cr
#'   Abstract function that must be implemented by concrete subclasses. `$predict_internal()` is called by `$predict()` after
#'   typechecking and works analogously to `$train_internal()`. Unlike `$train_internal()`, `$predict_internal()` should not modify
#'   the [`PipeOp`] in any way.\cr
#'   Just as `$train_internal()`, `$predict_internal()` should not be called by a user; instead, the `$predict()` method should be used.
#' * `print()` \cr
#'   () -> `NULL` \cr
#'   Prints the [`PipeOp`]s most salient information: `$id`, `$is_trained`, `$param_set$values`, `$input` and `$output`.
#' @examples
#' # example (bogus) PipeOp that returns the sum of two numbers during $train()
#' # as well as a letter of the alphabet corresponding to that sum during $predict().
#'
#' PipeOpSumLetter = R6::R6Class("sumletter",
#'   inherit = PipeOp,  # inherit from PipeOp
#'   public = list(
#'     initialize = function(id = "posum", param_vals = list()) {
#'       super$initialize(id, param_vals = param_vals,
#'         # declare "input" and "output" during construction here
#'         # training takes two 'numeric' and returns a 'numeric';
#'         # prediction takes 'NULL' and returns a 'character'.
#'         input = data.table::data.table(name = c("input1", "input2"),
#'           train = "numeric", predict = "NULL"),
#'         output = data.table::data.table(name = "output",
#'           train = "numeric", predict = "character")
#'       )
#'     },
#'
#'     # PipeOp deriving classes must implement train_internal and
#'     # predict_internal; each taking an input list and returning
#'     # a list as output.
#'     train_internal = function(input) {
#'       sum = input[[1]] + input[[2]]
#'       self$state = sum
#'       list(sum)
#'     },
#'
#'     predict_internal = function(input) {
#'       list(letters[self$state])
#'     }
#'   )
#' )
#' posum = PipeOpSumLetter$new()
#'
#' print(posum)
#'
#' posum$train(list(1, 2))
#' # note the name 'output' is the name of the output channel specified
#' # in the $output data.table.
#'
#' posum$predict(list(NULL, NULL))
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
      if (inherits(param_set, "ParamSet")) {
        private$.param_set = assert_param_set(param_set)
        private$.param_set_source = NULL
      } else {
        lapply(param_set, function(x) assert_param_set(eval(x)))
        private$.param_set_source = param_set
      }
      self$id = assert_string(id)

      self$param_set$values = insert_named(self$param_set$values, param_vals)
      self$input = assert_connection_table(input)
      self$output = assert_connection_table(output)
      self$packages = assert_character(packages, any.missing = FALSE, unique = TRUE)
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

    train = function(input) {
      require_namespaces(self$packages)

      if (every(input, is_noop)) {
        self$state = NO_OP
        return(named_list(self$output$name, NO_OP))
      }
      input = check_types(self, input, "input", "train")
      output = self$train_internal(input)
      output = check_types(self, output, "output", "train")
      output
    },
    predict = function(input) {
      # need to load packages in train *and* predict, because they might run in different R instances
      require_namespaces(self$packages)

      if (every(input, is_noop)) {
        return(named_list(self$output$name, NO_OP))
      }
      if (is_noop(self$state)) {
        stopf("Pipeop %s got NO_OP during train but no NO_OP during predict.", self$id)
      }

      input = check_types(self, input, "input", "predict")
      output = self$predict_internal(input)
      output = check_types(self, output, "output", "predict")
      output
    },
    train_internal = function(input) stop("abstract"),
    predict_internal = function(input) stop("abstract")
  ),

  active = list(
    id = function(val) {
      if (!missing(val)) {
        private$.id = val
        if (!is.null(private$.param_set)) {
          # private$.param_set may be NULL if it is constructed dynamically by active binding
          private$.param_set$set_id = val
        }
      }
      private$.id
    },
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        sourcelist = lapply(private$.param_set_source, function(x) eval(x))
        if (length(sourcelist) > 1) {
          private$.param_set = ParamSetCollection$new(sourcelist)
        } else {
          private$.param_set = sourcelist[[1]]
        }
        if (!is.null(self$id)) {
          private$.param_set$set_id = self$id
        }
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },
    predict_type = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$predict_type is read-only.")
        }
      }
      return(NULL)
    },
    innum = function() nrow(self$input),
    outnum = function() nrow(self$output),
    is_trained = function() !is.null(self$state),
    hash = function() {
      digest(list(class(self), self$id, self$param_set),
        algo = "xxhash64")
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      if (!is.null(private$.param_set_source)) {
        private$.param_set = NULL  # required to keep clone identical to original, otherwise tests get really ugly
        if (name == ".param_set_source") {
          value = lapply(value, function(x) {
            if (inherits(x, "R6")) x$clone(deep = TRUE) else x
          })
        }
      }
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    },
    .param_set = NULL,
    .param_set_source = NULL,
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
# @return an instance of 'data', possibly converted, with names added according to `$input`/`$output` "name" column.
check_types = function(self, data, direction, operation) {
  typetable = self[[direction]]
  if (direction == "input" && "..." %in% typetable$name) {
    assert_list(data, min.len = nrow(typetable) - 1)
    typetable = typetable[rep(1:.N, ifelse(get("name") == "...", length(data) - nrow(typetable) + 1, 1))]
  } else {
    assert_list(data, len = nrow(typetable))
  }
  for (idx in seq_along(data)) {
    typereq = typetable[[operation]][idx]
    if (typereq == "*") next
    if (typereq %in% class(data[[idx]])) next
    autoconverter = get_autoconverter(typereq)
    msg = ""
    if (!is.null(autoconverter)) {
      mlr3misc::require_namespaces(autoconverter$packages,
        sprintf("The following packages are required to convert object of class %s to class %s: %%s", class(data[[idx]])[1], typereq))
      tryCatch({
        data[[idx]] = autoconverter$fun(data[[idx]])
      }, error = function(e) msg <<- sprintf("\nConversion from given data to %s produced message:\n%s", typereq, e$message))
    }
    assert_class(data[[idx]], typereq,
      .var.name = sprintf("%s %s (\"%s\") of PipeOp %s%s",
        direction, idx, self$input$name[idx], self$id, msg))
  }
  names(data) = typetable$name
  data
}
