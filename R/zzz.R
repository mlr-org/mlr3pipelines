#' @import data.table
#' @import checkmate
#' @import mlr3
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom utils tail
#' @importFrom digest digest
#' @importFrom withr with_options
"_PACKAGE"


.onLoad = function(libname, pkgname) {
  # nocov start

  mlr_learners$add("graph", GraphLearner, required_args = "graph")

  backports::import(pkgname)

  mlr_reflections$constructors = rowwise_table(
    ~task_type, ~Task, ~Learner, ~Prediction, ~Measure,
    "regr", mlr3::TaskRegr, mlr3::LearnerRegr, mlr3::PredictionRegr, mlr3::MeasureRegr,
    "classif", mlr3::TaskClassif, mlr3::LearnerClassif, mlr3::PredictionClassif, mlr3::MeasureClassif)
  setkey(mlr_reflections$constructors, "task_type")
} # nocov end
