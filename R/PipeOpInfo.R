#' @title Customizable Information Printer
#'
#' @usage Customizable Information Printer that prints specific information about the object
#' @name mlr_pipeops_info
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`]
#'
#' @description
#' ...
#'
#' [additional information]
#'
#' @section Construction:
#' ```
#' PipeOpInfo$new(id = "info", collect_multiplicity = FALSE, log_target = ?WasistderDefault?)
#'
#' * `ìd` :: `character(1)`\cr
#'   Identifier of resulting object, default "info"
#' * `collect_multiplicity` :: `logical(1)`\cr
#'   If `TRUE`, the input is a [`Multiplicity`] collecting channel. This means, a
#'   [`Multiplicity`] input/output, instead of multiple normal inputs/outputs, is accepted and the members are aggregated.
#' * `log_target` :: `character(1)`\cr
#'   Has the form of <output>::<argument1>::<argument2>
#'
#' @section Input and Output Channels:
#' `PipeOpInfo` has one input channel called "input", it can take any type of input
#' `PipeOpInfo` has one output channel called "output", it can take any type of output
#'
#' @section State:
#' NULL
#'
#' @section Parameters:
#' NULL
#'
#' @section Internals:
#' ...
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `òriginal_printer` :: `list(4)` \cr
#'   The default printer, which is used when the user does not override it with customized printer settings.
#'   Printer settings are pre-defined for Objects of the class `Task`, `Prediction` and `NULL`.
#'   If the object on question does not belong to one of these classes, then the printer command labeled as `Default`
#'   will be utilized.
#' * `printer` ::
#'
#' @section Methods:
#'
#'
#' @references
#'
#' @family PipeOps
#'
#'
#'
#'
#'
#'



library(mlr3)
library(R6)
library(mlr3pipelines)
library(mlr3misc)
library(lgr)
library(checkmate)

# (.. 2 Spaces nach der Klammer
# wenn () auf der gleichen Zeile dann auch 2 Spaces nach (..
# Fehler wenn Objektklasse keine definierte Printer-Funktion
# Style Code mlr3 angucken und übernehmen
# environment(function) -- environment gibt uns die festgelegten Variablen und Funktionen in dem Environment als Liste wieder
# environment(function)$x = 99 Variable in der höheren Ebene anfassen
# wichtig wenn Funktion in der Funktion generiert wird, überlebt as environment ==> crate funktion verhindert dass das environment überlebt
# crate(function(y) x + y, x) -- somit wird x in nen container gepackt und wird nicht verloren gehen


PipeOpInfo = R6Class("PipeOpInfo",
                     inherit = PipeOp,
                     public = list(
                       # Felder, die zu Beginn definiert werden
                       original_printer = NULL,
                       printer = NULL,
                       collect_multiplicity = NULL,
                       log_target = NULL,
                       # Initialisierung
                       initialize = function(id = "info", printer = NULL, collect_multiplicity = FALSE, log_target = "lgr::mlr3/mlr3pipelines::info",
                                             param_vals = list()) {
                         browser()
                         assertString(log_target)
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
                         original_printer = list(
                           Task = crate(function(x) {
                             print(list(task = x, data = x$data()[, 1:min(10, ncol(x$data()))]), topn = 5)
                           }),
                           Prediction = crate(function(x) {list(prediction = x, score = tryCatch(x$score(), error = function(e) {}))}),
                           `NULL` = crate(function(x) "NULL"),
                           default = crate(function(x) print(x))
                         )
                         self$printer = insert_named(original_printer, printer)
                         # Collect multiplicity
                         self$collect_multiplicity = collect_multiplicity
                         # Log Target
                         self$log_target = log_target
                       }
                     ),
                     # Training
                     private = list(
                       .print_type = NULL,
                       .output = NULL,
                       .train = function(inputs) {
                         browser()
                         input_class = class(inputs[[1]])
                         leftmost_class =
                           if (any(input_class %in% names(self$printer))) {
                             input_class[input_class %in% names(self$printer)][[1]]
                           } else {
                             "default"
                           }
                         if (!("default" %in% names(self$printer))) {
                           stop("Object-class was not found and no default printer is available.")
                         }
                         specific_printer = self$printer[[leftmost_class]]
                         # Log Target
                         private$.output = function(inputs) {
                           split = strsplit(self$log_target, "::")[[1]]
                           if (split[[1]] == "lgr") {
                             assertString(self$log_target, pattern = "^[^:]+::[^:]+::[^:]+$")
                             logger = lgr::get_logger(split[[2]])
                             log_level = split[[3]]
                             logger$log(log_level, msg = capture.output(specific_printer(inputs[[1]])))
                           } else if (self$log_target == "cat") {
                             cat(capture.output(specific_printer(inputs[[1]])))
                           } else if (self$log_target == "message") {
                             message(capture.output(specific_printer(inputs[[1]])))
                           } else if (self$log_target == "warning") {
                             warning(capture.output(specific_printer(inputs[[1]])))
                           } else if (self$log_target == "none") {
                             print(specific_printer(inputs[[1]]))
                           } else {
                             stop(paste0("User-specified log_target is wrong",
                             sprintf("log_target was given as '%s'. But must have the form of either 'lgr::logger::level', 'cat', 'message' or 'warning'", log_target)))
                           }
                         }
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
poinfo_log_fatal$train(list(tsk("iris")))

poinfo_log_error = po("info", log_target = "lgr::mlr3/mlr3pipelines::error")
poinfo_log_error$train(list(tsk("iris")))

poinfo_log_warn = po("info", log_target = "lgr::mlr3/mlr3pipelines::warn")
poinfo_log_warn$train(list(tsk("iris")))

poinfo_log_info = po("info", log_target = "lgr::mlr3/mlr3pipelines::info")
poinfo_log_info$train(list(tsk("mtcars")))

poinfo_log_debug = po("info", log_target = "lgr::mlr3/mlr3pipelines::debug")
poinfo_log_debug$train(list(tsk("iris")))

poinfo_log_trace = po("info", log_target = "lgr::mlr3/mlr3pipelines::debug")
poinfo_log_trace$train(list(tsk("iris")))

# Nicht-Log
poinfo_cat = po("info", log_target = "cat")
resultat_cat = poinfo_cat$train(list(tsk("iris")))

poinfo_message = po("info", log_target = "message")
resultat_message = poinfo_message$train(list(tsk("iris")))

poinfo_warning = po("info", log_target = "warning")
resultat_warning = poinfo_warning$train(list(tsk("iris")))

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
resultat = poinfo_multiplicity_true$train(OVR)
poinfo_multiplicity_false = po("info", collect_multiplicity = FALSE,
                               printer = list(Multiplicity = function(x) lapply(x, FUN = function(y) {print(list(task = y, data = y$data()[, 1:min(10, ncol(y$data()))]), topn = 5)})))
resultat = poinfo_multiplicity_false$train(OVR)
poinfo_multiplicity_false




# Block 4 - Fragen zur Dokumentation
# $state bleibt bei NULL?
# internals
# fields ==> printer, collect_multiplicity, log_target?
# parameters ==> NULL


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
