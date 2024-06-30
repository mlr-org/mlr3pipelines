#' @include utils.R
#' @include mlr_pipeops.R
#'
#' @title PipeOp Base Class
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
#' for each channel against `$input` and then passed on to the `private$.train()` or `private$.predict()` functions. There the data is processed and
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
#' PipeOp$new(id, param_set = ps(), param_vals = list(), input, output, packages = character(0), tags = character(0))
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
#' * `tags` ::`character`\cr
#'   A set of tags associated with the `PipeOp`. Tags describe a PipeOp's purpose.
#'   Can be used to filter `as.data.table(mlr_pipeops)`. Default is `"abstract"`, indicating an abstract `PipeOp`.
#'
#' @section Internals:
#' [`PipeOp`] is an abstract class with abstract functions `private$.train()` and `private$.predict()`. To create a functional
#' [`PipeOp`] class, these two methods must be implemented. Each of these functions receives a named `list` according to
#' the [`PipeOp`]'s input channels, and must return a `list` (names are ignored) with values in the order of output
#' channels in `$output`. The `private$.train()` and `private$.predict()` function should not be called by the user;
#' instead, a `$train()` and `$predict()` should be used. The most convenient usage is to add the [`PipeOp`]
#' to a `Graph` (possibly as singleton in that `Graph`), and using the `Graph`'s `$train()` / `$predict()` methods.
#'
#' `private$.train()` and `private$.predict()` should treat their inputs as read-only. If they are [`R6`][R6::R6] objects,
#' they should be cloned before being manipulated in-place. Objects, or parts of objects, that are not changed, do
#' not need to be cloned, and it is legal to return the same identical-by-reference objects to multiple outputs.
#'
#' @section Fields:
#' * `id` :: `character`\cr
#'   ID of the [`PipeOp`]. IDs are user-configurable, and IDs of [`PipeOp`]s must be unique within a [`Graph`]. IDs of
#'   [`PipeOp`]s must not be changed once they are part of a [`Graph`], instead the [`Graph`]'s `$set_names()` method
#'   should be used.
#' * `packages` :: `character`\cr
#'   Packages required for the [`PipeOp`]. Functions that are not in base R should still be called using `::`
#'   (or explicitly attached using `require()`) in `private$.train()` *and* `private$.predict()`, but
#'   packages declared here are checked before any (possibly expensive) processing has started within a [`Graph`].
#' * `param_set` :: [`ParamSet`][paradox::ParamSet]\cr
#'   Parameters and parameter constraints. Parameter values that influence the functioning of `$train` and / or `$predict` are
#'   in the `$param_set$values` slot; these are automatically checked against parameter constraints in `$param_set`.
#' * `state` :: `any` | `NULL`\cr
#'   Method-dependent state obtained during training step, and usually required for the prediction step. This is `NULL`
#'   if and only if the [`PipeOp`] has not been trained. The `$state` is the *only* slot that can be reliably modified during
#'   `$train()`, because `private$.train()` may theoretically be executed in a different `R`-session (e.g. for parallelization).
#'   `$state` should furthermore always be set to something with copy-semantics, since it is never cloned. This is a limitation
#'   not of [`PipeOp`] or `mlr3pipelines`, but of the way the system as a whole works, together with [`GraphLearner`] and `mlr3`.
#' * input :: [`data.table`] with columns `name` (`character`), `train` (`character`), `predict` (`character`)\cr
#'   Input channels of [`PipeOp`]. Column `name` gives the names (and order) of values in the list given to
#'   `$train()` and `$predict()`. Column `train` is the (S3) class that an input object must conform to during
#'   training, column `predict` is the (S3) class that an input object must conform to during prediction. Types
#'   are checked by the [`PipeOp`] itself and do not need to be checked by `private$.train()` / `private$.predict()` code.\cr
#'   A special name is `"..."`, which creates a *vararg* input channel that accepts a variable number of inputs.\cr
#'   If a row has both `train` and `predict` values enclosed by square brackets ("`[`", "`]`), then this channel is
#'   [`Multiplicity`]-aware. If the [`PipeOp`] receives a [`Multiplicity`] value on these channels, this [`Multiplicity`]
#'   is given to the `.train()` and `.predict()` functions directly. Otherwise, the [`Multiplicity`] is transparently
#'   unpacked and the `.train()` and `.predict()` functions are called multiple times, once for each [`Multiplicity`] element.
#'   The type enclosed by square brackets indicates that only a [`Multiplicity`] containing values of this type are accepted.
#'   See [`Multiplicity`] for more information.
#' * output :: [`data.table`] with columns `name` (`character`), `train` (`character`), `predict` (`character`)\cr
#'   Output channels of [`PipeOp`], in the order in which they will be given in the list returned by `$train` and
#'   `$predict` functions. Column `train` is the (S3) class that an output object must conform to during training,
#'   column `predict` is the (S3) class that an output object must conform to during prediction. The [`PipeOp`] checks
#'   values returned by `private$.train()` and `private$.predict()` against these types specifications.\cr
#'   If a row has both `train` and `predict` values enclosed by square brackets ("`[`", "`]`), then this signals that the channel
#'   emits a [`Multiplicity`] of the indicated type. See [`Multiplicity`] for more information.
#' * `innum` :: `numeric(1)` \cr
#'   Number of input channels. This equals `nrow($input)`.
#' * `outnum` :: `numeric(1)` \cr
#'   Number of output channels. This equals `nrow($output)`.
#' * `is_trained` :: `logical(1)` \cr
#'   Indicate whether the [`PipeOp`] was already trained and can therefore be used for prediction.
#' * `tags` ::`character`\cr
#'   A set of tags associated with the `PipeOp`. Tags describe a PipeOp's purpose.
#'   Can be used to filter `as.data.table(mlr_pipeops)`.
#'   PipeOp tags are inherited and child classes can introduce additional tags.
#' * `hash` :: `character(1)` \cr
#'   Checksum calculated on the [`PipeOp`], depending on the [`PipeOp`]'s `class` and the slots `$id` and `$param_set$values`. If a
#'   [`PipeOp`]'s functionality may change depending on more than these values, it should inherit the `$hash` active
#'   binding and calculate the hash as `digest(list(super$hash, <OTHER THINGS>), algo = "xxhash64")`.
#' * `phash` :: `character(1)` \cr
#'   Checksum calculated on the [`PipeOp`], depending on the [`PipeOp`]'s `class` and the slots `$id` but ignoring `$param_set$values`. If a
#'   [`PipeOp`]'s functionality may change depending on more than these values, it should inherit the `$hash` active
#'   binding and calculate the hash as `digest(list(super$hash, <OTHER THINGS>), algo = "xxhash64")`.
#' * `.result` :: `list` \cr
#'   If the [`Graph`]'s `$keep_results` flag is set to `TRUE`, then the intermediate Results of `$train()` and `$predict()`
#'   are saved to this slot, exactly as they are returned by these functions. This is mainly for debugging purposes
#'   and done, if requested, by the [`Graph`] backend itself; it should *not* be done explicitly by `private$.train()` or `private$.predict()`.
#' * `man` :: `character(1)`\cr
#'   Identifying string of the help page that shows with `help()`.
#' * `properties` :: `character()`\cr
#'   The properties of the pipeop.
#'   Currently supported values are:
#'     * `"validation"`: the `PipeOp` can make use of the `$internal_valid_task` of an [`mlr3::Task`].
#'        This is for example used for `PipeOpLearner`s that wrap a `Learner` with this property, see [`mlr3::Learner`].
#'       `PipeOp`s that have this property, also have a `$validate` field, which controls whether to use the validation task,
#'        as well as a `$internal_valid_scores` field, which allows to access the internal validation scores after training.
#'     * `"internal_tuning"`: the `PipeOp` is able to internally optimize hyperparameters.
#'        This works analogously to the internal tuning implementation for [`mlr3::Learner`].
#'       `PipeOp`s with that property also implement the standardized accessor `$internal_tuned_values` and have at least one 
#'        parameter tagged with `"internal_tuning"`.
#'        An example for such a `PipeOp` is a `PipeOpLearner` that wraps a `Learner` with the `"internal_tuning"` property.
#'
#'   Programatic access to all available properties is possible via `mlr_reflections$pipeops$properties`.
#'
#' @section Methods:
#' * `train(input)`\cr
#'   (`list`) -> named `list`\cr
#'   Train [`PipeOp`] on `inputs`, transform it to output and store the learned `$state`. If the PipeOp is already
#'   trained, already present `$state` is overwritten. Input list is typechecked against the `$input` `train` column.
#'   Return value is a list with as many entries as `$output` has
#'   rows, with each entry named after the `$output` `name` column and class according to the `$output` `train` column.
#'   The workhorse function for training each [`PipeOp`] is the private
#'   `.train(input)`\cr: (named `list`) -> `list`\cr function.
#'   It's an Abstract function that must be implemented by concrete subclasses. `private$.train()` is called by `$train()` after
#'   typechecking. It must change the `$state` value to something non-`NULL` and return a list of transformed data according to
#'   the `$output` `train` column. Names of the returned list are ignored.\cr
#'   The `private$.train()` method should not be called by a user; instead, the `$train()` method should be used which does some
#'   checking and possibly type conversion.
#' * `predict(input)` \cr
#'   (`list`) -> named `list`\cr
#'   Predict on new data in `input`, possibly using the stored `$state`. Input and output are specified by `$input` and `$output`
#'   in the same way as for `$train()`, except that
#'   the `predict` column is used for type checking.
#'   The workhorse function for predicting in each using each [`PipeOp`] is
#'   `.predict(input)`\cr (named `list`) -> `list`\cr
#'   Abstract function that must be implemented by concrete subclasses. `private$.predict()` is called by `$predict()` after
#'   typechecking and works analogously to `private$.train()`. Unlike `private$.train()`, `private$.predict()` should not modify
#'   the [`PipeOp`] in any way.\cr
#'   Just as `private$.train()`, `private$.predict()` should not be called by a user; instead, the `$predict()` method should be used.
#' * `print()` \cr
#'   () -> `NULL` \cr
#'   Prints the [`PipeOp`]s most salient information: `$id`, `$is_trained`, `$param_set$values`, `$input` and `$output`.
#' * `help(help_type)` \cr
#'   (`character(1)`) -> help file\cr
#'   Displays the help file of the concrete `PipeOp` instance. `help_type` is one of `"text"`, `"html"`, `"pdf"` and behaves
#'   as the `help_type` argument of R's `help()`.
#'
#' @section Inheriting:
#' To create your own `PipeOp`, you need to overload the `private$.train()` and `private$.test()` functions.
#' It is most likely also necessary to overload the `$initialize()` function to do additional initialization.
#' The `$initialize()` method should have at least the arguments `id` and `param_vals`, which should be passed on to `super$initialize()` unchanged.
#' `id` should have a useful default value, and `param_vals` should have the default value `list()`, meaning no initialization of hyperparameters.
#'
#' If the `$initialize()` method has more arguments, then it is necessary to also overload the `private$.additional_phash_input()` function.
#' This function should return either all objects, or a hash of all objects, that can change the function or behavior of the `PipeOp` and are independent
#' of the class, the id, the `$state`, and the `$param_set$values`. The last point is particularly important: changing the `$param_set$values` should
#' *not* change the return value of `private$.additional_phash_input()`.
#'
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
#'     }
#'   ),
#'   private = list(
#'     # PipeOp deriving classes must implement .train and
#'     # .predict; each taking an input list and returning
#'     # a list as output.
#'     .train = function(input) {
#'       sum = input[[1]] + input[[2]]
#'       self$state = sum
#'       list(sum)
#'     },
#'     .predict = function(input) {
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
#' @template seealso_pipeopslist
#' @export
PipeOp = R6Class("PipeOp",
  public = list(
    packages = NULL,
    state = NULL,
    input = NULL,
    output = NULL,
    .result = NULL,
    tags = NULL,
    properties = NULL,

    initialize = function(id, param_set = ps(), param_vals = list(), input, output, packages = character(0), tags = "abstract", properties = character(0)) {
      if (inherits(param_set, "ParamSet")) {
        private$.param_set = assert_param_set(param_set)
        private$.param_set_source = NULL
      } else {
        lapply(param_set, function(x) assert_param_set(eval(x)))
        private$.param_set_source = param_set
      }
      self$id = assert_string(id)

      self$properties = assert_subset(properties, mlr_reflections$pipeops$properties)
      self$param_set$values = insert_named(self$param_set$values, param_vals)
      self$input = assert_connection_table(input)
      self$output = assert_connection_table(output)
      self$packages = union("mlr3pipelines", assert_character(packages, any.missing = FALSE, min.chars = 1L))
      self$tags = assert_subset(tags, mlr_reflections$pipeops$valid_tags)
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
      assert_list(input, .var.name = sprintf("input to PipeOp %s's $train()", self$id))
      self$state = NULL  # reset to untrained state first
      require_namespaces(self$packages)

      if (every(input, is_noop)) {
        self$state = NO_OP
        return(named_list(self$output$name, NO_OP))
      }
      unpacked = unpack_multiplicities(input, multiplicity_type_nesting_level(self$input$train), self$input$name, self$id)
      if (!is.null(unpacked)) {
        return(evaluate_multiplicities(self, unpacked, "train", NULL))
      }
      input = check_types(self, input, "input", "train")
      on.exit({self$state = NULL})  # if any of the following fails, make sure to reset self$state
      withCallingHandlers({
        output = private$.train(input)
      }, error = function(e) {
        e$message = sprintf("%s\nThis happened PipeOp %s's $train()", e$message, self$id)
        stop(e)
      }, warning = function(w) {
        w$message = sprintf("%s\nThis happened PipeOp %s's $train()", w$message, self$id)
        warning(w)
        invokeRestart("muffleWarning")
      })
      if (!is.null(self$state) && !is.null(private$.state_class)) {
        class(self$state) = c(private$.state_class, class(self$state))
      }

      output = check_types(self, output, "output", "train")
      on.exit()  # don't reset state any more
      output
    },
    predict = function(input) {
      assert_list(input, .var.name = sprintf("input to PipeOp %s's $predict()", self$id))

      # need to load packages in train *and* predict, because they might run in different R instances
      require_namespaces(self$packages)

      if (every(input, is_noop)) {
        return(named_list(self$output$name, NO_OP))
      }
      if (is_noop(self$state)) {
        stopf("Pipeop %s got NO_OP during train but no NO_OP during predict.", self$id)
      }
      unpacked = unpack_multiplicities(input, multiplicity_type_nesting_level(self$input$predict), self$input$name, self$id)
      if (!is.null(unpacked)) {
        return(evaluate_multiplicities(self, unpacked, "predict", self$state))
      }
      input = check_types(self, input, "input", "predict")
      withCallingHandlers({
        output = private$.predict(input)
      }, error = function(e) {
        e$message = sprintf("%s\nThis happened PipeOp %s's $predict()", e$message, self$id)
        stop(e)
      }, warning = function(w) {
        w$message = sprintf("%s\nThis happened PipeOp %s's $predict()", w$message, self$id)
        warning(w)
        invokeRestart("muffleWarning")
      })
      output = check_types(self, output, "output", "predict")
      output
    },
    help = function(help_type = getOption("help_type")) {
      parts = strsplit(self$man, split = "::", fixed = TRUE)[[1]]
      match.fun("help")(parts[[2]], package = parts[[1]], help_type = help_type)
    }
  ),

  active = list(
    id = function(val) {
      if (!missing(val)) {
        private$.id = val
        if (paradox_info$is_old && !is.null(private$.param_set)) {
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
        if (paradox_info$is_old && !is.null(self$id)) {
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
        stop("$predict_type is read-only.")
      }
      return(NULL)
    },
    innum = function() nrow(self$input),
    outnum = function() nrow(self$output),
    is_trained = function() !is.null(self$state),
    hash = function() {
      digest(list(class(self), self$id, lapply(self$param_set$values, function(val) {
        # ideally we would just want to hash `param_set$values`, but one of the values
        # could be an R6 object with a `$hash` slot as well, in which case we take that
        # slot's value. This is to avoid different hashes from essentially the same
        # objects.
        # In the following we also avoid accessing `val$hash` twice, because it could
        # potentially be an expensive AB.
        if (is.environment(val) && !is.null({vhash = get0("hash", val, mode = "any", inherits = FALSE, ifnotfound = NULL)})) {
          vhash
        } else {
          val
        }
      }), private$.additional_phash_input()), algo = "xxhash64")
    },
    phash = function() {
      digest(list(class(self), self$id, private$.additional_phash_input()), algo = "xxhash64")
    },
    man = function(x) {
      if (!missing(x)) stop("man is read-only")
      paste0(topenv(self$.__enclos_env__)$.__NAMESPACE__.$spec[["name"]], "::", class(self)[[1]])
    },
    label = function(x) {
      if (!missing(x)) stop("label is read-only")
      if (is.null(private$.label)) {
        helpinfo = self$help()
        helpcontent = NULL
        if (inherits(helpinfo, "help_files_with_topic") && length(helpinfo)) {
          ghf = get(".getHelpFile", mode = "function", envir = getNamespace("utils"))
          helpcontent = ghf(helpinfo)
        } else if (inherits(helpinfo, "dev_topic")) {
          helpcontent = tools::parse_Rd(helpinfo$path)
        }
        if (is.null(helpcontent)) {
          private$.label = "LABEL COULD NOT BE RETRIEVED"
        } else {
          private$.label = Filter(function(x) identical(attr(x, "Rd_tag"), "\\title"), helpcontent)[[1]][[1]][1]
        }
      }
      private$.label
    }
  ),

  private = list(
    .state_class = NULL,
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
    .train = function(input) stop("abstract"),
    .predict = function(input) stop("abstract"),
    .additional_phash_input = function() {
      if (is.null(self$initialize)) return(NULL)
      initformals <- names(formals(args(self$initialize)))
      if (!test_subset(initformals, c("id", "param_vals"))) {
        warningf("PipeOp %s has construction arguments besides 'id' and 'param_vals' but does not overload the private '.additional_phash_input()' function.

The hash and phash of a PipeOp must differ when it represents a different operation; since %s has construction arguments that could change the operation that is performed by it, it is necessary for the $hash and $phash to reflect this. `.additional_phash_input()` should return all the information (e.g. hashes of encapsulated items) that should additionally be hashed; read the help of ?PipeOp for more information.

This warning will become an error in the future.", class(self)[[1]], class(self)[[1]])
      }
    },
    .param_set = NULL,
    .param_set_source = NULL,
    .label = NULL,
    .id = NULL
  )
)

# Asserts that input and output tables are correctly specified
# @param table: `data.table`: either input or output
assert_connection_table = function(table) {
  varname = deparse(substitute(table))
  assert_data_table(table, .var.name = varname, min.rows = 1)
  assert_names(names(table), permutation.of = c("name", "train", "predict"), .var.name = varname)
  assert_character(table$name, any.missing = FALSE, unique = TRUE, .var.name = paste0("'name' column in ", varname))
  if (!all(multiplicity_type_nesting_level(table$train) == multiplicity_type_nesting_level(table$predict))) {
    stop("Multiplicity during train and predict conflicts.")
  }
  table
}

# Checks that data conforms to the type specifications given
# Handles multiplicities: if a type is in square brackets ("[<TYPE>]"), then a "Multiplicity" that contains the type is checked.
# Yes, this can handle nested multiplicities: "[[<TYPE>]]" etc. works.
# @param data: `list of any`: is either the input or output given to a train/predict function. it is checked to be a *list* first
#   and then to have the types as given by the `$input` or `$output` data.table
# @param direction: `character(1)`: is either `"input"` or `"output"`
# @param operation: `character(1)`: is either `"train"` or `"predict"`
# @return an instance of data, possibly converted, with names added according to `$input`/`$output` "name" column
check_types = function(self, data, direction, operation) {
  typetable = self[[direction]]
  description = sprintf("%s of PipeOp %s's $%s()", direction, self$id, operation)
  if (direction == "input" && "..." %in% typetable$name) {
    assert_list(data, min.len = nrow(typetable) - 1, .var.name = description)
    typetable = typetable[rep(1:.N, ifelse(get("name") == "...", length(data) - nrow(typetable) + 1, 1))]
  } else {
    assert_list(data, len = nrow(typetable), .var.name = description)
  }

  check_item = function(data_element, typereq, varname) {
    if (multiplicity_type_nesting_level(typereq, varname)) {
      # unpack multiplicity
      assert_multiplicity(data_element, varname)
      typereq = substr(typereq, 2, nchar(typereq) - 1)
      for (midx in seq_along(data_element)) {
        # recursively call check_item for each multiplicity-item
        data_element[midx] = list(check_item(data_element[[midx]], typereq, sprintf("Multiplicity element %s of %s", midx, varname)))
      }
      # done checking, return early.
      return(data_element)
    }
    if (is.Multiplicity(data_element)) {
      stopf("Problem with %s: %s contained Multiplicity when it shouldn't have.", varname, data_element)
    }
    if (typereq == "*") return(data_element)
    if (typereq %in% class(data_element)) return(data_element)
    autoconverter = get_autoconverter(typereq)
    msg = ""
    if (!is.null(autoconverter)) {
      mlr3misc::require_namespaces(autoconverter$packages,
        sprintf("The following packages are required to convert object of class %s to class %s: %%s.", class(data_element)[1], typereq))
      msg = tryCatch({
        data_element = autoconverter$fun(data_element)
        ""
      }, error = function(e) sprintf("\nConversion from given data to %s produced message:\n%s.", typereq, e$message))
    }
    assert_class(data_element, typereq, .var.name = paste0(varname, msg))
  }

  for (idx in seq_along(data)) {
    data[idx] = list(check_item(data[[idx]], typetable[[operation]][[idx]],
      varname = sprintf("%s %s (\"%s\") of PipeOp %s's $%s()",
        direction, idx, self[[direction]]$name[[idx]], self$id, operation)))
  }
  names(data) = typetable$name
  data
}

# get the number of `[` `]` nestings of a variable name
# E.g. multiplicity_type_nesting_level(c("Task", "[Prediction]", "[[*]]")) --> c(0, 1, 2)
# @param str: `character`: type descriptors to check
# @param varname `character(1)`: where the value is found, used to print error message
# @return `integer`
multiplicity_type_nesting_level = function(str, varname) {
  beginning = map_int(gregexpr("^\\[*", str), attr, "match.length")
  end = map_int(gregexpr("\\]*$", str), attr, "match.length")
  if (any(beginning != end)) {
    stopf("Invalid type(s) %s in %s: square bracket mismatch.", str_collapse(str[beginning != end]), varname)
  }
  beginning
}

# unpacks pipeop arguments with multiplicities, if necessary, into (possibly named) lists that can be iterated over
# @param input: `list` of multiplicities: multiplicities to unpack
# @param expected_nesting_level: `integer`: expected nesting level of the multiplicities
# @param inputnames: `character`: names of the resulting lists
# @param poid: `character(1)`: character id of the PipeOp
# @return `list`
unpack_multiplicities = function(input, expected_nesting_level, inputnames, poid) {
  assert_list(input)
  unpacking = mapply(multiplicity_nests_deeper_than, input, expected_nesting_level)
  if (!any(unpacking)) {
    return(NULL)  # no unpacking
  }
  prototype_index = which(unpacking)[[1]]
  prototype = input[[prototype_index]]
  if (sum(unpacking) > 1) {
    # check that all elements being unpacked are the same multiplicity (length, names)
    # in the future we may be a bit more lax here and allow "vectorization" or cartesian products, but
    # for that we may rather want to have explicit pipeops
    for (comparing_index in which(unpacking)[-1]) {
      comparing = input[[comparing_index]]
      if (length(comparing) != length(prototype) || !identical(names(comparing), names(prototype))) {
        stopf("Input of %s has bad multiplicities: %s has different length and/or names than %s.",
          poid, inputnames[[prototype_index]], inputnames[[comparing_index]])
      }
    }
  }
  set_names(map(seq_along(prototype), function(idx) {
    map_at(input, unpacking, function(x) x[[idx]])
  }), names(prototype))
}

# evaluate multiplicities
# @param self: typically a `PipeOp`
# @param unpacked: `list` of unpacked multiplicities
# @param evalcall: either `train` or `predict`
# @param instate: typically `self$state`
evaluate_multiplicities = function(self, unpacked, evalcall, instate) {
  force(instate)
  on.exit({self$state = instate})
  if (!is.null(instate)) {
    if (!is.Multiplicity(instate)) {
      stopf("PipeOp %s received multiplicity input on %s but state was not a multiplicity.", self$id, evalcall)
    }
    if (length(instate) != length(unpacked) || !identical(names(instate), names(unpacked))) {
      stopf("PipeOp %s received multiplicity input on %s but state had different length / names than input.", self$id, evalcall)
    }
  }
  result = imap(unpacked, function(input, reference) {
    self$state = if (!is.null(instate)) instate[[reference]]
    list(output = self[[evalcall]](input), state = self$state)
  })
  on.exit({self$state = as.Multiplicity(map(result, "state"))})
  if (length(unpacked) == 0) {
    # if input was length-0 multiplicity, then we need to construct output ourselves, because
    # 'result' is just an empty list missing the necessary info about output channels.
    # (not necessary for 'state', because it is a list of states, whereas 'output' is a
    # list (entry for each input multiplicity entry) of lists (entry for each output channel) of outputs
    sapply(self$output$name, function(x) as.Multiplicity(list()), simplify = FALSE)
  } else {

    map(transpose_list(map(result, "output")), as.Multiplicity)
  }
}
