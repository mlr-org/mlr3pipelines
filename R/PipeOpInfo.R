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
#' NULL
#'
#' @section Parameters:
#' NULL
#'
#' @section Internals:
#' Was wird hier genau beschrieben? Also was ist der Zweck dieses Abschnitts
#' https://github.com/mlr-org/mlr3pipelines/blob/0b5c4b766995334369d423ab337462843d5a4b30/R/PipeOpSmoteNC.R#L50
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `òriginal_printer` :: `list(4)` \cr
#'   The default printer, which is used when the user does not override it with customized printer settings.
#'   Printer settings are pre-defined for Objects of the class `Task`, `Prediction` and `NULL`.
#'   If the object on question does not belong to one of these classes, then the printer command labeled as `Default`
#'   will be utilized.
#'
#'
#'
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`]
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



library(mlr3)
library(R6)
library(mlr3pipelines)
library(mlr3misc)
library(lgr)
library(checkmate)


PipeOpInfo = R6Class("PipeOpInfo",
                     inherit = PipeOp,
                     public = list(
                       # Felder, die zu Beginn definiert werden
                       printer = NULL,
                       collect_multiplicity = NULL,
                       log_target = NULL,
                       # Initialisierung
                       initialize = function(id = "info", printer = NULL, collect_multiplicity = FALSE, log_target = "lgr::mlr3/mlr3pipelines::info",
                                             param_vals = list()) {
                         #browser()
                         # Entscheidung den assert so früh zu verorten (also unter initialize direkt) damit dem User direkt klar wird, wenn das log_target nicht konform ist
                         assertString(log_target, pattern = "(cat|none|warning|message|^[^:]+::[^:]+::[^:]+$)")
                         # String definieren der nix bedeutet zB "none"
                         intype = "*"
                         outtype = "*"
                         if (collect_multiplicity) {
                           intype = sprintf("[%s]", intype)
                           outtype = sprintf("[%s]", outtype)
                         }
                         # Initialisierung der Parameter aus PipeOp
                         super$initialize(id, param_vals = param_vals,
                                          input = data.table(name = "input", train = intype, predict = intype),
                                          output = data.table(name = "output", train = outtype, predict = outtype),
                                          tags = "ensemble")
                         # Überschreiben des Default Printers mit Eingabe des Users
                         private$.original_printer = list(
                           Task = crate(function(x) {
                             print(list(task = x, data = x$data()[, 1:min(10, ncol(x$data()))]), topn = 5)
                           }),
                           Prediction = crate(function(x) {list(prediction = x, score = tryCatch(x$score(), error = function(e) {}))}),
                           `NULL` = crate(function(x) "NULL"),
                           default = crate(function(x) print(x))
                         )
                         self$printer = insert_named(private$.original_printer, printer)
                         # Collect multiplicity
                         self$collect_multiplicity = collect_multiplicity
                         # Log Target
                         self$log_target = log_target
                       }
                     ),
                     # Training
                     private = list(
                       .original_printer = NULL,
                       .output = function(inputs) {
                         #browser()
                         input_class = class(inputs[[1]])
                         leftmost_class =
                           if (any(input_class %in% names(self$printer))) {
                             input_class[input_class %in% names(self$printer)][[1]]
                           } else {
                             "default"
                           }
                         ##################### ist das notwendig, original_printer wurde nach private verlegt und somit sollte ein default-printer immer verfügbar sein
                         if (!("default" %in% names(self$printer))) {
                           stop("Object-class was not found and no default printer is available.")
                         }
                         specific_printer = self$printer[[leftmost_class]]
                         log_target_split = strsplit(self$log_target, "::")[[1]]
                         if (log_target_split[[1]] == "lgr") {
                           logger = lgr::get_logger(log_target_split[[2]])
                           log_level = log_target_split[[3]]
                           logger$log(log_level, msg = capture.output(specific_printer(inputs[[1]])))
                         } else if (identical(log_target_split, "cat")) {
                           cat(capture.output(specific_printer(inputs[[1]])))
                         } else if (identical(log_target_split, "message")) {
                           message(capture.output(specific_printer(inputs[[1]])))
                         } else if (identical(log_target_split, "warning")) {
                           warning(capture.output(specific_printer(inputs[[1]])))
                         } else if (identical(log_target_split, "none")) {
                           print(inputs[[1]])
                         } else {
                           stop(paste0("User-specified log_target is wrong",
                             sprintf("log_target was given as '%s'. But must have the form of either 'lgr::logger::level', 'cat', 'message' or 'warning'", log_target))
                           )
                         }
                       },
                       .train = function(inputs) {
                         private$.output(inputs)
                         inputs
                       },
                       # Prediction
                       .predict = function(inputs) {
                         private$.output(inputs)
                         inputs
                       }
                     )
)

mlr_pipeops$add("info", PipeOpInfo)

# Default
poinfo_default = po("info")

resultat = poinfo_default$train(list(tsk("mtcars")))
prediction = poinfo_default$predict(list(tsk("penguins")))

# None
poinfo_none = po("info", log_target = "none")
resultat = poinfo_none$train(list(tsk("mtcars")))
prediction = poinfo_none$predict(list(tsk("penguins")))

# Log Levels
poinfo_log_fatal = po("info", log_target = "lgr::mlr3/mlr3pipelines::fatal")
resultat = poinfo_log_fatal$train(list(tsk("iris")))

poinfo_log_error = po("info", log_target = "lgr::mlr3/mlr3pipelines::error")
resultat = poinfo_log_error$train(list(tsk("iris")))

poinfo_log_warn = po("info", log_target = "lgr::mlr3/mlr3pipelines::warn")
resultat = poinfo_log_warn$train(list(tsk("iris")))

poinfo_log_info = po("info", log_target = "lgr::mlr3/mlr3pipelines::info")
resultat = poinfo_log_info$train(list(tsk("mtcars")))

poinfo_log_debug = po("info", log_target = "lgr::mlr3/mlr3pipelines::debug")
resultat = poinfo_log_debug$train(list(tsk("iris")))

poinfo_log_trace = po("info", log_target = "lgr::mlr3/mlr3pipelines::debug")
resultat = poinfo_log_trace$train(list(tsk("iris")))

# Nicht-Log
poinfo_cat = po("info", log_target = "cat")
resultat_cat = poinfo_cat$train(list(tsk("iris")))

poinfo_message = po("info", log_target = "message")
resultat_message = poinfo_message$train(list(tsk("iris")))

poinfo_warning = po("info", log_target = "warning")
resultat_warning = poinfo_warning$train(list(tsk("iris")))

# PipeOp, der einen String zurückgibt
poinfo_string = po("info", printer = list(Task = function(x) "Hello"))
resultat = poinfo_string$train(list(tsk("iris")))

# Log falsch spezifiziert
poinfo_wrong = po("info", log_target = "wrongtextwrongtextwrong")
resultat = poinfo_wrong$train(list(tsk("iris")))


# Multiplicity Object - OVR
library(mlr3)
task = tsk("iris")
po = po("ovrsplit")
OVR = po$train(list(task))
OVR
class(OVR[[1]])

# Collect Multiplicity
poinfo_multiplicity_true = po("info", collect_multiplicity = TRUE,
                              printer = list(Multiplicity = function(x) lapply(x, FUN = function(y) {print(list(task = y, data = y$data()[, 1:min(10, ncol(y$data()))]), topn = 5)})))
poinfo_multiplicity_true
resultat = poinfo_multiplicity_true$train(OVR)

poinfo_multiplicity_false = po("info", collect_multiplicity = FALSE,
                               printer = list(Multiplicity = function(x) lapply(x, FUN = function(y) {print(list(task = y, data = y$data()[, 1:min(10, ncol(y$data()))]), topn = 5)})))
poinfo_multiplicity_false
resultat = poinfo_multiplicity_false$train(OVR)


# Logger einrichten
logger = lgr::get_logger("mlr3/mlr3pipelines")
logger_file = tempfile(fileext = ".info")
logger$add_appender(AppenderFile$new(logger_file), name = "logfile")
logger$remove_appender("logfile")

# Fragen zur Dokumentation
#https://github.com/mlr-org/mlr3pipelines/blob/0b5c4b766995334369d423ab337462843d5a4b30/R/PipeOpTuneThreshold.R#L57
#Was sind hier genau die Methoden, es sind ja zusätzliche Funktionen in private außer train/predict definiert

# Fragen zu Prediction-Objekt
# wie kreiert man ein Prediction Objekt wo die truth spalte fehlt









# Standard
poinfo = po("info")
poinfo$train(list(tsk("iris")))

# Printer that returns a list works
poinfo_nrow_ncol = po("info", printer = list(
  Task = function(x) {list(nrow = x$nrow, ncol = x$ncol, distr = table(x$data()[, "Species"]))},
  TaskClassif = function(x) {list(nrow = x$nrow, ncol = x$ncol, distr = table(x$data()[, "Species"]))})
)
poinfo_nrow_ncol$train(list(tsk("iris")))

poinfo = po("info", printer = list(
  Task = function(x) {list(a = "Task not be printed; THIS IS A TASK")},
  TaskClassif = function(x) {list(a = "TaskClassif not be printed. THIS IS A TASK CLASSIF")
  }
))



#Examples
poinfo$train(list(tsk("iris")))
poinfo$train(list(tsk("mtcars")))
poinfo$train(list(prediction))
poinfo$train(list(NULL))
poinfo$train(list("abc"))

# Fragen


# Beispiel - "printer that returns a list works"
poinfo_nrow_ncol_table_mtcars = po("info", printer = list(
  Task = function(x) {list(nrow = x$nrow, ncol = x$ncol, distr = table(x$data()[, x$target_names]))},
  TaskClassif = function(x) {list(nrow = x$nrow, ncol = x$ncol, distr = table(x$data()[, x$target_names]))})
)
resultat = poinfo_nrow_ncol_table_mtcars$train(list(tsk("mtcars")))




# Prediction Object
na.omit(data(PimaIndiansDiabetes2, package = "mlbench"))
tsk1 = as_task_classif(PimaIndiansDiabetes2, target = "diabetes", positive = "pos")
splits = partition(tsk1, ratio = 0.8)
lrn_classif = lrn("classif.rpart", predict_type = "prob")
lrn_classif$train(tsk1, row_ids = splits$train)
prediction = lrn_classif$predict(tsk1, row_ids = splits$test)
prediction$score(msr("classif.ce"))


## Cursor anschauen
