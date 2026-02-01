#' @title Customizable Information Printer
#'
#' @usage NULL
#' @name mlr_pipeops_info
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`]
#'
#' @description
#' `PipeOpInfo` prints its input to the console or a logger in a customizable way.
#' Users can define how specific object classes should be displayed using custom printer functions.
#'
#' @section Construction:
#' ```
#' PipeOpInfo$new(id = "info", collect_multiplicity = FALSE, log_target = "lgr::mlr3/mlr3pipelines::info")
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default "info"
#' * `printer` :: `list` \cr
#'   Optional mapping from object classes to printer functions. Custom functions override default printer-functions.
#' * `collect_multiplicity` :: `logical(1)`\cr
#'   If `TRUE`, the input is a [`Multiplicity`] collecting channel. [`Multiplicity`] input/output is accepted and the members are aggregated.
#' * `log_target` :: `character(1)`\cr
#'   Specifies how the input object is printed to the console. By default it is
#'   directed to a logger, whose address can be customized using the form
#'   `<output>::<argument1>::<argument2>`. Otherwise it can be printed
#'   as "message", "warning" or "cat". When set to "none", no customized
#'   information about the object will be printed.
#'
#' @section Input and Output Channels:
#' `PipeOpInfo` has one input channel called "input", it can take any type of input (`*`).
#' `PipeOpInfo` has one output channel called "output", it can take any type of output (`*`).
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Internals:
#' `PipeOpInfo` forwards its input unchanged, but prints information about it
#' depending on the `printer` and `log_target` settings.
#'
#' @section Fields:
#' Fields inherited from `PipeOp`, as well as:
#' * `printer` :: `list`\cr
#'   Mapping of object classes to printer functions. Includes printer-specifications for `Task`, `Prediction`, `NULL`. Otherwise object is printed as is.
#' * `log_target` :: `character(1)` \cr
#'   Specifies current output target.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' poinfo = po("info")
#' poinfo$train(list(tsk("mtcars")))
#' poinfo$predict(list(tsk("mtcars")))
#'
#' # Specify customized console output for Task-objects
#' poinfo = po("info", log_target = "cat",
#'   printer = list(Task = function(x) list(head_data = head(x$data()), nrow = nrow(x$data())))
#' )
#'
#' poinfo$train(list(tsk("iris")))
#' poinfo$predict(list(tsk("iris")))
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
#'
#'

PipeOpInfo = R6Class("PipeOpInfo",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "info", printer = NULL, collect_multiplicity = FALSE, log_target = "lgr::mlr3/mlr3pipelines::info", param_vals = list()) {
      assertString(log_target, pattern = "^(cat|none|warning|message|lgr::[^:]+::[^:]+)$")
      inouttype = "*"
      if (collect_multiplicity) {
        inouttype = sprintf("[%s]", inouttype)
      }
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = inouttype, predict = inouttype),
        output = data.table(name = "output", train = inouttype, predict = inouttype)
        #tag = "debug"
      )
      original_printer = list(
        Task = crate(function(x) {
          row_preview = head(x$row_ids, 10L)
          col_preview = head(c(x$target_names, x$feature_names), 10L)
          data_preview = x$data(rows = row_preview, cols = col_preview)
          list(
            task = x,
            data_preview = data_preview
          )
        }),
        Prediction = crate(function(x) {
          tryCatch(list(prediction = x, score = x$score()), error = function(e) {list(prediction = x)})
        }),
        `NULL` = crate(function(x) "NULL"),
        default = crate(function(x) x)
      )
      private$.printer = insert_named(original_printer, printer)
      private$.log_target = log_target
    }
  ),
  active = list(
    printer = function(rhs) {
      if (!missing(rhs)) stop("printer is read only.")
      private$.printer
    },
    log_target = function(rhs) {
      if (!missing(rhs)) stop("log_target is read only.")
      private$.log_target
    }
  ),
  private = list(
    .printer = NULL,
    .log_target = NULL,
    .output = function(inputs, stage) {
      input_class = class(inputs[[1]])
      leftmost_class =
        if (any(input_class %in% names(private$.printer))) {
          input_class[input_class %in% names(private$.printer)][[1]]
        } else {
          "default"
        }
      if (!("default" %in% names(private$.printer))) {
        stop("Object-class was not found and no default printer is available.")
      }
      specific_printer = private$.printer[[leftmost_class]]
      log_target_split = strsplit(private$.log_target, "::")[[1]]
      stage_string = sprintf("Object passing through PipeOp %s - %s", self$id, stage)
      print_string = utils::capture.output({
        cat(stage_string, "\n\n")
        specific_printer(inputs[[1]])
      })
      message_text = paste(print_string, collapse = "\n")
      if (log_target_split[[1]] == "lgr") {
        logger = lgr::get_logger(log_target_split[[2]])
        log_level = log_target_split[[3]]
        logger$log(log_level, msg = message_text)
      } else if (private$.log_target == "cat") {
        cat(message_text)
      } else if (private$.log_target == "message") {
        message(message_text)
      } else if (private$.log_target == "warning") {
        warning(message_text)
      } else if (private$.log_target == "none") {
      } else {
        stopf("Invalid log_target '%s'.", private$.log_target)
      }
    },
    .train = function(inputs, stage = "Training") {
      self$state = list()
      private$.output(inputs, stage)
      inputs
    },
    .predict = function(inputs, stage = "Prediction") {
      private$.output(inputs, stage)
      inputs
    },
    .additional_phash_input = function() {
      list(printer = self$printer, log_target = self$log_target)
    }
  )
)

mlr_pipeops$add("info", PipeOpInfo)
