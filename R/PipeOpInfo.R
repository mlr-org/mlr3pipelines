#' @title Customizable Information Printer
#'
#' @usage Customizable Information Printer that prints specific information about the object
#' @name mlr_pipeops_info
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`]
#'
#' @description
#' Prints the given input in a customized way.
#'
#' [additional information]
#'
#' @section Construction:
#' ```
#' PipeOpInfo$new(id = "info", collect_multiplicity = FALSE, log_target = "lgr::mlr3/mlr3pipelines::info")
#' ```
#' * `ìd` :: `character(1)`\cr
#'   Identifier of resulting object, default "info"
#' * `printer` :: `list(???)` \cr
#'   User input, specified printer-functions defined for a new object-classes or used to override their counterparts in the `original_printer`
#' * `collect_multiplicity` :: `logical(1)`\cr
#'   If `TRUE`, the input is a [`Multiplicity`] collecting channel. This means, a
#'   [`Multiplicity`] input/output, instead of multiple normal inputs/outputs, is accepted and the members are aggregated.
#' * `log_target` :: `character(1)`\cr
#'   Specifies how the output is printed, can either be assigned to a logger with a specified level, or can be printer in the
#'   format "message", "warning" or "cat". When the log_target is specified as "none", the input will be printed as is.
#'   Has either he form <output>::<argument1>::<argument2> for logger output otherwise "message", "warning", "cat" or none.
#'
#' @section Input and Output Channels:
#' `PipeOpInfo` has one input channel called "input", it can take any type of input (*)
#' `PipeOpInfo` has one output channel called "output", it can take any type of output (*)
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' NULL
#'
#' @section Internals:
#' Zusätzliche Info
#' Was wird hier genau beschrieben? Also was ist der Zweck dieses Abschnitts
#' https://github.com/mlr-org/mlr3pipelines/blob/0b5c4b766995334369d423ab337462843d5a4b30/R/PipeOpSmoteNC.R#L50
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `original_printer` :: `list(4)` \cr
#'   The default printer, which is used when the user does not override it with customized printer settings.
#'   Printer settings are pre-defined for Objects of the class `Task`, `Prediction` and `NULL`.
#'   If the object on question does not belong to one of these classes, then the printer command labeled as `Default`
#'   will be utilized.
#'
#'
#'
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#'
#' @examples
#' library("mlr3")
#' poinfo = po("info")
#' poinfo$train(list(tsk("mtcars")))
#' poinfo$predict(list(tsk("penguins")))
#'
#'
#' @references
#'
#' @family PipeOps
#' @template
#' @include
#' @export
#'
#'

PipeOpInfo = R6Class("PipeOpInfo",
                     inherit = PipeOp,
                     public = list(
                       initialize = function(id = "info", printer = NULL, collect_multiplicity = FALSE, log_target = "lgr::mlr3/mlr3pipelines::info", param_vals = list()) {
                         assertString(log_target, pattern = "^(cat|none|warning|message|lgr::[^:]+::[^:]+)$")
                         #browser()
                         inouttype = "*"
                         if (collect_multiplicity) {
                           inouttype = sprintf("[%s]", inouttype)
                         }
                         super$initialize(id, param_vals = param_vals,
                                          input = data.table(name = "input", train = inouttype, predict = inouttype),
                                          output = data.table(name = "output", train = inouttype, predict = inouttype)
                         ) # which tag is appropriate
                         original_printer = list(
                           Task = crate(function(x) {
                             print(list(task = x, data = x$data()[, 1:min(10, ncol(x$data()))]), topn = 5)
                           }),
                           Prediction = crate(function(x) {
                             print(tryCatch(list(prediction = x, score = x$score()), error = function(e) {list(prediction = x)}), topn = 5)
                           }),
                           `NULL` = crate(function(x) "NULL"),
                           default = crate(function(x) print(x))
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
                         #browser()
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
                         stage_string = sprintf("Object passing through PipeOp Infoprint - %s", stage) #Infoprint stattdessen self$id mit %s
                         #capture.output({cat(stage_string, "\n\n"); specific_printer(inputs[[1]])}), collapse = "\n") -- als Variable deklarieren
                         if (log_target_split[[1]] == "lgr") {
                           logger = get_logger(log_target_split[[2]])
                           log_level = log_target_split[[3]]
                           logger$log(log_level, msg = capture.output({cat(stage_string, "\n\n"); specific_printer(inputs[[1]])}))
                         } else if (private$.log_target == "cat") {  # private$.log_target == "cat"
                           {cat(stage_string, "\n\n"); specific_printer(inputs[[1]])}
                         } else if (private$.log_target == "message") {
                           message(paste(capture.output({
                             cat(stage_string, "\n\n")
                             specific_printer(inputs[[1]])
                           }), collapse = "\n"))
                         } else if (private$.log_target == "warning") {
                           warning(paste(capture.output({cat(stage_string, "\n\n"); specific_printer(inputs[[1]])}), collapse = "\n"))
                         } else if (private$.log_target == "none") {
                         } else {
                           stopf("Invalid log_target '%s'.", log_target)
                         }
                       },
                       .train = function(inputs, stage = "Training") {
                         private$.output(inputs, stage)
                         inputs
                       },
                       .predict = function(inputs, stage = "Prediction") {
                         private$.output(inputs, stage)
                         inputs
                       }
                     )
)

mlr_pipeops$add("info", PipeOpInfo)

