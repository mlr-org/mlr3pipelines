context("Dictionary")

# we check that all pipeops that are exported are also in the dictionary, and can be constructed from there.
test_that("Dictionary contains all PipeOps", {
  skip_if_not_installed("mlr3filters")
  skip_if_not_installed("rpart")
  skip_on_cran()

  oldwarn = options(warn = 2)
  # make sure all caching private member vars are extended
  inflate = function(x) {
    x$label
    x$param_set$values$content$label
    x
  }

  # abstract pipeops that don't need to be in mlr_pipeops
  abstracts = c("PipeOp", "PipeOpEnsemble", "PipeOpTaskPreproc", "PipeOpTaskPreprocSimple", "PipeOpImpute", "PipeOpTargetTrafo")

  # param set values not hashable, because functions
  pval_unhashable = "missind"

  # constructor-args that have no defaults
  initargs = list(
    PipeOpBranch = list(options = 2),
    PipeOpChunk = list(outnum = 2),
    PipeOpCopy = list(outnum = 2),
    PipeOpFeatureUnion = list(innum = 2),
    PipeOpImputeLearner = list(learner = mlr_learners$get("classif.rpart")),
    PipeOpLearner = list(learner = mlr_learners$get("classif.rpart")),
    PipeOpLearnerCV = list(learner = mlr_learners$get("classif.rpart")),
    PipeOpLearnerPICVPlus = list(learner = mlr_learners$get("regr.rpart")),
    PipeOpLearnerQuantiles = list(learner = mlr_learners$get("regr.featureless")),
    PipeOpClassifAvg = list(innum = 2),
    PipeOpRegrAvg = list(innum = 2),
    PipeOpUnbranch = list(options = 2),
    PipeOpFilter = list(filter = mlr3filters::FilterVariance$new(), param_vals = list(filter.nfeat = 1)),
    PipeOpMultiplicityExply = list(outnum = 2))

  # The PipeOps that may have a default ID different from the mlr_pipeops key
  unequal_id = c("PipeOpLearner", "PipeOpLearnerCV",  "PipeOpLearnerPICVPlus","PipeOpLearnerQuantiles", "PipeOpFilter")

  # function that recursively checks if an R6 Class Generator generates a subclass of PipeOp (*without* constructing it)
  inherits_from_pipeop = function(r6cg) {
    if (r6cg$classname == "PipeOp") {
      return(TRUE)
    }
    upper = r6cg$get_inherit()
    if (is.null(upper)) {
      return(FALSE)
    }
    inherits_from_pipeop(upper)
  }

  # get a list of objects that are exported by the mlr3pipelines package
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
  # now 'pipeops' contains the names of all PipeOps that need to be present in the dictionary.

  # for each pipeop that *should* be in the dictionary, get the corresponding mlr_pipeops "key"
  # (or __NOT_FOUND__ if the pipeop is not in the dictionary)
  dictnames = map_chr(pipeops, function(pipe) {
    c(names(mlr_pipeops$items)[map_lgl(mlr_pipeops$items, function(gen) {
      identical(gen$value, get(pipe, pkgenv))
    })], "__NOT_FOUND__")[1]
  })

  expect("__NOT_FOUND__" %nin% dictnames, "Not all exported non-abstract PipeOps are in mlr_pipeops")
  tmp = setdiff(mlr_pipeops$keys(), dictnames)
  expect(length(tmp) == 0, sprintf("PipeOps in mlr_pipeops that are not exported: %s", paste(tmp, collapse = ",")))

  # as.atomic: converts non-atomic things to NULL
  as.atomic = function(x) if (is.atomic(x)) x

  # the loop now checks whether we can construct each pipeop from the dictionary *and* by itself
  for (idx in seq_along(dictnames)) {
    pogen = get(pipeops[idx], pkgenv)  # the constructor, as found in the package namespace
    dictname = dictnames[idx]  # the "key" in the mlr_pipeops dictionary
    args = initargs[[pipeops[idx]]] %??% list()  # the required arguments, if any. e.g. 'outnum' for PipeOpCopy.

    test_obj = do.call(pogen$new, args)  # test_obj: PipeOp object, constructed from constructor



    # check that mlr_pipeops key is the default ID
    if (pipeops[idx] %nin% unequal_id) {
      expect_equal(test_obj$id, dictname)
    }

    # check that mlr_pipeops$get() gives the same object as PipeOpXXX$new() does
    other_obj = do.call(mlr_pipeops$get, c(list(dictname), args))
    expect_equal(other_obj, test_obj, info = paste(dictname, "$new test"))
    if (!dictname %in% pval_unhashable) {
      expect_equal(other_obj$hash, test_obj$hash, info = paste(dictname, "$new test"))
    }
    expect_equal(other_obj$phash, test_obj$phash, info = paste(dictname, "$new test"))

    # check that ID can be changed
    args$id = "TESTID"
    other_obj = do.call(mlr_pipeops$get, c(list(dictname), args))
    expect_false(isTRUE(all.equal(other_obj, test_obj)), info = paste(dictname, "$new id test"))
    expect_false(isTRUE(all.equal(other_obj$hash, test_obj$hash)), info = paste(dictname, "$new id test"))
    expect_false(isTRUE(all.equal(other_obj$phash, test_obj$phash)), info = paste(dictname, "$new id test"))
    test_obj$id = "TESTID"
    other_obj = inflate(do.call(mlr_pipeops$get, c(list(dictname), args)))
    expect_equal(other_obj, test_obj, info = paste(dictname, "$new id test 2"))
    if (!dictname %in% pval_unhashable) {
      expect_equal(other_obj$hash, test_obj$hash, info = paste(dictname, "$new id test 2"))
    }
    expect_equal(other_obj$phash, test_obj$phash, info = paste(dictname, "$new id test 2"))
    expect_equal(inflate(do.call(pogen$new, args)), test_obj, info = dictname)


    tops = test_obj$param_set
    # we now check if hyperparameters can be changed through construction
    # we do this by automatically generating a hyperparameter value that deviates from the automatically constructed one.
    # However, for ParamUty we can't do that, so if there are only 'ParamUty' parameter we skip this part.
    eligibleparams = which(
      tops$class != "ParamUty" &
      # filter out discrete params with only one level, or the numeric parameters with $lower == $upper
      # Note that numeric parameters have 0 levels, and discrete parameters have $lower == $upper (== NA)
      (
        (!is.na(tops$lower) & tops$lower != tops$upper) |
        (is.finite(tops$nlevels) & tops$nlevels > 1)
      )
    )
    if (length(eligibleparams)) {
      testingparam = tops$ids()[[eligibleparams[[1]]]]

      # we want to construct an object where the parameter value is *different* from the value it gets on construction by default.
      # For this we take a few candidate values and `setdiff` the original value
      origval = as.atomic(test_obj$param_set$values[[testingparam]])
      if (tops$class[[testingparam]] %in% c("ParamLgl", "ParamFct")) {
        candidates = tops$levels[[testingparam]]
      } else {
        candidates = Filter(function(x) is.finite(x) && !is.na(x),
          c(tops$lower[[testingparam]], tops$upper[[testingparam]], tops$lower[[testingparam]] + 1, 0, origval + 1))
      }
      val = setdiff(candidates, origval)[1]

      # construct the `param_vals = list(PARNAME = PARVAL)` construction argument
      args$param_vals = list(val)
      names(args$param_vals) = testingparam

      # check that the constructed object is different from the test_obj, but setting the test_obj's parameter
      # makes them equal again.
      dict_constructed = do.call(mlr_pipeops$get, c(list(dictname), args))
      gen_constructed = do.call(pogen$new, args)
      expect_false(isTRUE(all.equal(dict_constructed, test_obj)), dictname)
      expect_false(isTRUE(all.equal(dict_constructed$hash, test_obj$hash)), dictname)
      # phash should be independent of this!
      expect_true(isTRUE(all.equal(dict_constructed$phash, test_obj$phash)), dictname)

      test_obj$param_set$values[[testingparam]] = val
      expect_equal(touch(dict_constructed), test_obj)
      expect_equal(inflate(touch(gen_constructed)), test_obj)

    }
    # $help() works and gives expected result
    expect_equal(test_obj$man, paste0("mlr3pipelines::", pipeops[idx]), info = paste(dictname, test_obj$man, "vs", paste0("mlr3pipelines::", pipeops[idx])))

    # $label can be retrieved
    expect_string(test_obj$label)
    help_metainfo = paste(capture.output(print(str(test_obj$help()))), sep = "\n")
    expect_false(grepl("^LABEL COULD NOT BE RETRIEVED$", test_obj$label), info = paste(dictname, help_metainfo, sep = "\n"))

    if (identical(help, utils::help)) {  # different behaviour if pkgload / devtools are doing help vs. vanilla R help()
      expect_equal(
        c(help(pipeops[idx], package = "mlr3pipelines")),
        c(test_obj$help()), info = paste("help for", dictname)
      )

      expect_equal(
          # use c() to strip all attributes; they indicate the actual help()-call which is obviously different here.
        c(help(paste0("mlr_pipeops_", dictname), package = "mlr3pipelines")),
        c(test_obj$help()),
        info = paste("help for", dictname, "II")
      )
    } else {
      expect_equal(
        help(pipeops[idx], package = "mlr3pipelines"),
        test_obj$help(), info = paste("help for", dictname)
      )
    }
  }
  options(warn = oldwarn$warn)
})

test_that("data.table of pipeops looks as it should", {
  potable = as.data.table(mlr_pipeops)

  expect_set_equal(colnames(potable),
    c("key", "label", "packages", "tags", "feature_types",
      "input.num", "output.num",
      "input.type.train", "input.type.predict",
      "output.type.train", "output.type.predict"))

  expect_equal(nrow(potable), length(mlr_pipeops$keys()))

  expect_set_equal(potable$key, mlr_pipeops$keys())

  expect_equal(potable["branch"]$output.num, NA_integer_)
  expect_equal(potable["unbranch"]$input.num, NA_integer_)

  expect_equal(potable["branch"]$output.type.train, list("*"))
  expect_equal(potable["featureunion"]$input.type.train, list("Task"))

  expect_equal(potable["learner"]$output.type.train, list("NULL"))
  expect_equal(potable["learner_cv"]$input.type.train, list("TaskClassif"))
  expect_equal(potable["learner_pi_cvplus"]$input.type.train, list("TaskRegr"))
  expect_equal(potable["learner_quantiles"]$input.type.train, list("TaskRegr"))
})

test_that("GraphLearner is in mlr_learners", {
  skip_if_not_installed("rpart")

  expect_data_table(as.data.table(mlr_learners))  # can construct mlr_learners table

  # unfortunately we don't add GraphLearner to mlr_learners
  #expect_equal(
  #  mlr_learners$get("graph", graph = PipeOpLearner$new(lrn("classif.rpart"))),
  #  GraphLearner$new(Graph$new()$add_pipeop(PipeOpLearner$new(lrn("classif.rpart"))))
  #)

  # FIXME: depends on mlr-org/mlr3#328
  # expect_error(mlr_learners$get("graph"), "'graph'.*'graph'")  # Needs the argument 'graph' to construct 'graph'

})


test_that("mlr_graphs dictionary", {
  expect_r6(mlr_graphs)
  dt = as.data.table(mlr_graphs)
  expect_data_table(dt, col.names = "unique")
  expect_true("key" %in% colnames(dt))
})

test_that("Cannot add pipeops with keys that invalidates the convenience for id incrementation", {
  copy = mlr_pipeops$clone(deep = TRUE)
  expect_error(copy$add("name_1", PipeOp), regexp = "grepl")
})

test_that("as.data.table(mlr_pipeops) works when a pipeop can not be constructed", {
  PipeOpError = R6Class("PipeOpError", inherit = PipeOp, public = list(
    initialize = function() {
      stop("This PipeOp can not be constructed")
    }
  ))
  dt_before = as.data.table(mlr_pipeops)

  # No NAs in the data.table of mlr3pipelines' own PipeOps
  expect_false(any(is.na(unlist(
    dt_before[, c("key", "label", "packages", "tags", "input.type.train", "input.type.predict",
      "output.type.train", "output.type.predict"), with = FALSE])
  )))

  mlr_pipeops$add("error_pipeop", PipeOpError)
  expect_warning({dt_after = as.data.table(mlr_pipeops)}, "could not be constructed.*error_pipeop")
  mlr_pipeops$remove("error_pipeop")

  expect_equal(dt_after[key == "error_pipeop"],
    # use weird data.table constructor since we can't use 'key' in the data.table() call
    setkeyv(data.table()[, `:=`(key = "error_pipeop", label = NA_character_, packages = list(NA_character_),
      tags = list(NA_character_), feature_types = list(NA_character_),
      input.num = NA_integer_, output.num = NA_integer_,
      input.type.train = list(NA_character_), input.type.predict = list(NA_character_),
      output.type.train = list(NA_character_), output.type.predict = list(NA_character_))][], "key"))

})
