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

# (.. 2 Spaces nach der Klammer
# wenn () auf der gleichen Zeile dann auch 2 Spaces nach (..
# Fehler wenn Objektklasse keine definierte Printer-Funktion
# Style Code mlr3 angucken und übernehmen
# environment(function) -- environment gibt uns die festgelegten Variablen und Funktionen in dem Environment als Liste wieder
# environment(function)$x <- 99 Variable in der höheren Ebene anfassen
# wichtig wenn Funktion in der Funktion generiert wird, überlebt as environment ==> crate funktion verhindert dass das environment überlebt
# crate(function(y) x + y, x) -- somit wird x in nen container gepackt und wird nicht verloren gehen


PipeOpInfo = R6Class("PipeOpInfo",
                     inherit = PipeOp,
                     public = list(
                       # Felder, die zu Beginn definiert werden
                       original_printer = NULL,
                       printer = NULL,
                       collect_multiplicity = NULL,
                       logger = NULL,
                       log_level = NULL,
                       log_target_func = NULL,
                       inprefix = NULL,
                       # Initialisierung
                       initialize = function(id = "info", printer = NULL, collect_multiplicity = FALSE, log_target = "lgr::mlr3/mlr3pipelines::info",
                                             param_vals = list()) {
                         browser()
                         #assertString(logtarget)
                         # String definieren der nix bedeutet zB "none"
                         intype = "*"
                         outtype = "*"
                         #private$.collect = assert_flag(collect_multiplicity)
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
                         #browser()
                         original_printer = list(
                           Task = crate(function(x) {
                             #oldOption = getOption("datatable.print.topn")
                             #options(datatable.print.topn = 10)
                             #on.exit(options(datatable.print.topn = oldOption))
                             list(task = x, data = x$data()[, 1:min(10, ncol(x$data()))])
                           }),
                           Prediction = crate(function(x) {list(prediction = x, score = tryCatch(x$score(), error = function(e) {}))}),
                           `NULL` = crate(function(x) "NULL"),
                           default = crate(function(x) print(x))
                         )
                         self$printer = insert_named(original_printer, printer)
                         # Collect multiplicity
                         self$collect_multiplicity = collect_multiplicity
                         # Log Target
                         if (!is.null(log_target)) {
                           split = strsplit(log_target, "::")[[1]]
                         } else {
                           split = "print"
                         }
                         if (split[1] == "lgr") {
                           #logger_file <- tempfile(fileext = ".info")
                           # assert --- es gibt 2x"::"  ".*::.*"
                           logger = lgr::get_logger(split[2])
                           #if (!is.null(logger$appenders$logfile)) {
                           #  logger$remove_appender("logfile")
                           #}
                           #logger$add_appender(AppenderFile$new(logger_file), name = "logfile")
                           log_level = split[3]
                           log_target_func = crate(function(x) self$logger$log(log_level, msg = capture.output(x)), log_level)
                         } else if (log_target == "cat") { ## so umformulieren mit log_target
                           log_target_func = crate(function(x) cat(capture.output(x)))
                         } else if (split[[1]] == "message") {
                           log_target_func = function(x) message(capture.output(x))
                         } else if (split[[1]] == "warning") {
                           log_target_func = function(x) warning(capture.output(x))
                         } else if (split == "print") {
                           log_target_func = function(x) print(x)
                         } else {stop("log_target is wrong")} ## sprintf(log target was given as -- but has to look like this: --)
                         self$log_target_func = log_target_func
                         #private$.log_target_func = log_target_func # private field damit der User nicht draufzugreift
                       }
                     ),
                     # Training und Prediction
                     private = list(
                       .train = function(inputs) {
                         browser()
                         #class(inputs[[1]]) als Hilfsvariable
                         specific_class =
                           if (any(class(inputs[[1]]) %in% names(self$printer))) {
                             class(inputs[[1]])[class(inputs[[1]]) %in% names(self$printer)][[1]]
                           } else {
                             "default"
                           }
                         # fehlermeldung wenn es den default-printer nicht gibt ---> class nicht gefunden & kein Default printer
                         do.call(self$log_target_func, list(self$printer[[specific_class]](inputs[[1]])))
                         inputs
                       },
                       .predict = function(inputs) {
                         inputs
                       }
                     )
)
#private$.output (inputs)
mlr_pipeops$add("info", PipeOpInfo)


# Block 1 - Normale Use Cases

# Default Printer für Task
# Da wir print(list(Task = x, Data = print.data.table(...))) haben fallen die in print.data.table(...) spezifizierten Einstellungen
# auf den default zurück, also man kann zBsp topn nicht spezifizieren
taskiris <- tsk("iris")
print(taskiris$data(), topn = 5)
print(list(Task = taskiris, Data = taskiris$data()), topn = 5) ## diese Darstellung für Print-Task verwenen

# Beispiel - "printer that returns a list works"
poinfo_nrow_ncol_table_mtcars <- po("info", printer = list(
  Task = function(x) {list(nrow = x$nrow, ncol = x$ncol, distr = table(x$data()[, "mpg"]))},
  TaskClassif = function(x) {list(nrow = x$nrow, ncol = x$ncol, distr = table(x$data()[, "mpg"]))})
)
poinfo_nrow_ncol_table_mtcars$train(list(tsk("mtcars")))


# Block 2 - Target Outputs

# Print Outputs Beispiele
# Default
poinfo_default <- po("info")
resultat = poinfo_default$train(list(tsk("iris")))

# default sollte "lgr::mlr3/mlr3pipelines::info" sein

# Log Levels
poinfo_log_fatal <- po("info", log_target = "lgr::mlr3/mlr3pipelines::fatal")
poinfo_log_fatal$train(list(tsk("iris")))
poinfo_log_error <- po("info", log_target = "lgr::mlr3/mlr3pipelines::error")
poinfo_log_error$train(list(tsk("iris")))
poinfo_log_warn <- po("info", log_target = "lgr::mlr3/mlr3pipelines::warn")
poinfo_log_warn$train(list(tsk("iris")))


poinfo_log_info <- po("info", log_target = "lgr::mlr3/mlr3pipelines::info")
poinfo_log_info$train(list(tsk("mtcars")))


poinfo_log_debug <- po("info", log_target = "lgr::mlr3/mlr3pipelines::debug")
poinfo_log_debug$train(list(tsk("iris")))
poinfo_log_trace <- po("info", log_target = "lgr::mlr3/mlr3pipelines::debug")
poinfo_log_trace$train(list(tsk("iris")))

# Nicht-Log Szenarien
poinfo_cat <- po("info", log_target = "cat")
poinfo_cat$train(list(tsk("iris")))
poinfo_message <- po("info", log_target = "message")
poinfo_message$train(list(tsk("iris")))
poinfo_warning <- po("info", log_target = "warning")
poinfo_warning$train(list(tsk("iris")))

# Frage - Line 110ff. soll capture.output(x) verwendet werden oder nicht?

# Block 3 - Collect Multiplicity

poinfo_multiplicity_true = po("info", collect_multiplicity = TRUE, printer = list(Multiplicity = function(x) ...))
poinfo_multiplicity_true
poinfo_multiplicity_true$train(OVR)
poinfo_multiplicity_false = po("info", collect_multiplicity = FALSE)
poinfo_multiplicity_false
poinfo_multiplicity_false

# Multiplicity Object - OVR
library(mlr3)
task = tsk("iris")
po = po("ovrsplit")
OVR = po$train(list(task))
OVR
class(OVR[[1]])
poinfo_multiplicity_true$train(OVR)

poinfo_nrow_ncol$train(OVR)


# Block 4 - Fragen zur Dokumentation
# $state bleibt bei NULL?
# internals
# fields ==> printer, collect_multiplicity, log_target?
# parameters ==> NULL


# Standard
poinfo <- po("info")
poinfo$train(list(tsk("iris")))

# Printer that returns a list works
poinfo_nrow_ncol <- po("info", printer = list(
  Task = function(x) {list(nrow = x$nrow, ncol = x$ncol, distr = table(x$data()[, "Species"]))},
  TaskClassif = function(x) {list(nrow = x$nrow, ncol = x$ncol, distr = table(x$data()[, "Species"]))})
)
poinfo_nrow_ncol$train(list(tsk("iris")))

poinfo <- po("info", printer = list(
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






# Prediction Object
na.omit(data(PimaIndiansDiabetes2, package = "mlbench"))
tsk1 <- as_task_classif(PimaIndiansDiabetes2, target = "diabetes", positive = "pos")
splits <- partition(tsk1, ratio = 0.8)
lrn_classif <- lrn("classif.rpart", predict_type = "prob")
lrn_classif$train(tsk1, row_ids = splits$train)
prediction <- lrn_classif$predict(tsk1, row_ids = splits$test)
prediction$score(msr("classif.ce"))


# Multiplicity Object
library("mlr3")
task = tsk("iris")
po = po("replicate", param_vals = list(reps = 3))
multiplicity_object <- po$train(list(task))
class(multiplicity_object)

poinfo <- po("info", collect_multiplicity = TRUE)
poinfo$train(multiplicity_object)


## Cursor anschauen
