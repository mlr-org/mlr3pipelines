context("Dictionary")

test_that("Dictionary contains all PipeOps", {

  # abstract pipeops that don't need to be in mlr_pipeops
  abstracts = c("PipeOp", "PipeOpEnsemble", "PipeOpTaskPreproc", "PipeOpTaskPreprocSimple")

  # constructor-args that have no defaults
  initargs = list(
      PipeOpBranch = list(options = 2),
      PipeOpChunk = list(outnum = 2),
      PipeOpCopy = list(outnum = 2),
      PipeOpFeatureUnion = list(innum = 2),
      PipeOpLearner = list(learner = mlr_learners$get("classif.rpart")),
      PipeOpLearnerCV = list(learner = mlr_learners$get("classif.rpart")),
      PipeOpMajorityVote = list(innum = 2),
      PipeOpModelAvg = list(innum = 2),
      PipeOpNlOptMajorityVote = list(innum = 2),
      PipeOpNlOptModelAvg = list(innum = 2),
      PipeOpUnbranch = list(options = 2),
      PipeOpFilter = list(filter = mlr3featsel::FilterVariance$new(), param_vals = list(nfeat = 1)))

  # The PipeOps that may have a default ID different from the mlr_pipeops key
  unequal_id = c("PipeOpLearner", "PipeOpLearnerCV", "PipeOpFilter")

  inherits_from_pipeop = function(r6cg) {
    if (r6cg$classname == "PipeOp") {
      return(TRUE)
    }
    upper = r6cg$get_inherit()
    if (is.null(upper)) {
      return(FALSE)
    }
    return(inherits_from_pipeop(upper))
  }

  nspath = dirname(system.file("NAMESPACE", package = "mlr3pipelines"))
  exports = parseNamespaceFile(basename(nspath), dirname(nspath))$exports

  pkgenv = asNamespace("mlr3pipelines")
  pipeops = Filter(function(x) {
    objgen = get(x, pkgenv)
    "R6ClassGenerator" %in% class(objgen) &&
      inherits_from_pipeop(objgen)
  }, ls(pkgenv, all.names = TRUE))

  pipeops = intersect(pipeops, exports)

  pipeops = setdiff(pipeops, abstracts)

  dictnames = map_chr(pipeops, function(pipe) {
    c(names(mlr_pipeops$items)[map_lgl(mlr_pipeops$items, function(gen) {
      identical(gen$value, get(pipe, pkgenv))
    })], "__NOT_FOUND__")[1]
  })

  expect("__NOT_FOUND__" %nin% dictnames, "Not all exported non-abstract PipeOps are in mlr_pipeops")
  expect(length(setdiff(mlr_pipeops$keys(), dictnames)) == 0, "Not all PipeOps in mlr_pipeops are exported.")

  as.atomic = function(x) if (is.atomic(x)) x

  for (idx in seq_along(dictnames)) {

    pogen = get(pipeops[idx], pkgenv)
    dictname = dictnames[idx]
    args = initargs[[pipeops[idx]]] %??% list()

    test_obj = do.call(pogen$new, args)

    # check that mlr_pipeops key is the default ID
    if (pipeops[idx] %nin% unequal_id) {
      expect_equal(test_obj$id, dictname)
    }

    # check that mlr_pipeops$get gives the right object

    expect_equal(do.call(mlr_pipeops$get, c(list(dictname), args)), test_obj)

    # check that ID can be changed

    args$id = "TESTID"
    expect_false(isTRUE(all.equal(do.call(mlr_pipeops$get, c(list(dictname), args)), test_obj)))
    test_obj$id = "TESTID"
    expect_equal(do.call(mlr_pipeops$get, c(list(dictname), args)), test_obj)
    expect_equal(do.call(pogen$new, args), test_obj)

    eligibleparams = test_obj$param_set$params[test_obj$param_set$class != "ParamUty"]

    if (length(eligibleparams)) {
      singletons = map_lgl(eligibleparams, function(p) {
        length(p$levels) < 2 && isTRUE(all.equal(p$lower, p$upper))
      })
      testingparam = eligibleparams[[which(!singletons)[1]]]

      if (testingparam$class %in% c("ParamLgl", "ParamFct")) {
        val = setdiff(testingparam$levels, as.atomic(testingparam$default))[1]
      } else {
        val = setdiff(c(testingparam$lower, testingparam$upper), as.atomic(testingparam$default))[1]
      }

      args$param_vals = list(val)
      names(args$param_vals) = testingparam$id

      expect_false(isTRUE(all.equal(do.call(mlr_pipeops$get, c(list(dictname), args)), test_obj)))
      test_obj$param_set$values[[testingparam$id]] = val
      expect_equal(do.call(mlr_pipeops$get, c(list(dictname), args)), test_obj)
      expect_equal(do.call(pogen$new, args), test_obj)
    }
  }

})

test_that("data.table of pipeops looks as it should", {

  potable = as.data.table(mlr_pipeops)

  expect_equal(colnames(potable),
    c("id", "packages", "input.num", "output.num",
      "input.type.train", "input.type.predict",
      "output.type.train", "output.type.predict"))

  expect_equal(nrow(potable), length(mlr_pipeops$keys()))

  expect_set_equal(potable$id, mlr_pipeops$keys())

  expect_equal(potable["branch"]$output.num, NA_integer_)
  expect_equal(potable["unbranch"]$input.num, NA_integer_)

  expect_equal(potable["branch"]$output.type.train, list("*"))
  expect_equal(potable["featureunion"]$input.type.train, list("Task"))

  expect_equal(potable["learner"]$output.type.train, list("NULL"))
  expect_equal(potable["learner_cv"]$input.type.train, list("Task"))

})

