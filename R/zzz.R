#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom utils tail
#' @importFrom digest digest
#' @importFrom withr with_options
"_PACKAGE"


.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)

  mlr_pipeops <<- DictionaryPipeOp$new()
  mlr_pipeops$add("backuplearner", PipeOpBackupLearner)
  mlr_pipeops$add("balancesample", PipeOpBalanceSample)
  mlr_pipeops$add("branch", PipeOpBranch, list("N"))
  mlr_pipeops$add("chunk", PipeOpChunk, list("N"))
  mlr_pipeops$add("copy", PipeOpCopy, list("N"))
  mlr_pipeops$add("encode", PipeOpEncode)
  mlr_pipeops$add("featureunion", PipeOpFeatureUnion, list("N"))
  mlr_pipeops$add("filter", PipeOpFilter, list(R6Class("Filter", public = list(id = "dummyfilter", param_set = ParamSet$new()))$new()))
  mlr_pipeops$add("impute", PipeOpImpute)
  mlr_pipeops$add("learner_cv", PipeOpLearnerCV, list(R6Class("Learner", public = list(id = "learner_cv", param_set = ParamSet$new()))$new()))
  mlr_pipeops$add("learner", PipeOpLearner, list(R6Class("Learner", public = list(id = "learner", param_set = ParamSet$new()))$new()))
  mlr_pipeops$add("majorityvote", PipeOpMajorityVote, list("N"))
  mlr_pipeops$add("modelavg", PipeOpModelAvg, list("N"))
  mlr_pipeops$add("nloptmajorityvote", PipeOpNlOptMajorityVote, list("N"))
  mlr_pipeops$add("nloptmodelavg", PipeOpNlOptModelAvg, list("N"))
  mlr_pipeops$add("null", PipeOpNULL)
  mlr_pipeops$add("pca", PipeOpPCA)
  mlr_pipeops$add("scale", PipeOpScale)
  mlr_pipeops$add("subsample", PipeOpSubsample)
  mlr_pipeops$add("unbranch", PipeOpUnbranch, list("N"))
} #nocov end
