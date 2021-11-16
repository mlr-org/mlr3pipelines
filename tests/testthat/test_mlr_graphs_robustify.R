context("ppl - pipeline_robustify")

test_that("Robustify Pipeline", {
  skip_on_cran()
  lrn = lrn("classif.rpart")

  # complete data, numeric
  tsk = tsk("iris")
  p = pipeline_robustify(task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("removeconstants_postrobustify" %in% names(p$pipeops))
  expect_true("fixfactors" %nin% names(p$pipeops))
  expect_true(length(p$pipeops) == 3)

  tsk = tsk("pima")
  # missings with scaling (rpart can do missings)
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("removeconstants_postrobustify" %in% names(p$pipeops))

  # with fct
  dt = data.table("fct" = factor(rep_len(letters[1:3], tsk$nrow)))
  tsk$cbind(dt)
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("encode" %nin% names(p$pipeops))
  # assuming rpart can not do fcts
  lrn$feature_types = c("integer", "numeric")
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("encode" %in% names(p$pipeops))

  # missing fcts, assuming rpart can not do missings
  lrn$properties = c("multiclass", "twoclass")
  dt =  data.table("fct2" = factor(rep_len(c(letters[1:3], NA), tsk$nrow)))
  tsk$cbind(dt)
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("imputehist", "missind", "encode", "imputeoor") %in% names(p$pipeops)))

  # no scaling
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("imputehist", "missind") %in% names(p$pipeops)))

  # test on mixed, no missings
  tsk = tsk("boston_housing")
  lrn = lrn("regr.rpart")
  p = ppl("robustify", task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("encode" %nin% names(p$pipeops))
  expect_true("missind" %nin% names(p$pipeops))
  expect_true("imputeoor" %nin% names(p$pipeops))

  # logical impute_missings
  p = ppl("robustify", task = tsk, learner = lrn, impute_missings = TRUE) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("imputehist", "missind", "imputeoor") %in% names(p$pipeops)))

  # no task
  p = pipeline_robustify() %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("char_to_fct", "imputehist", "missind", "imputeoor",
    "collapsefactors", "encode") %in% names(p$pipeops)))

  p = ppl("robustify", impute_missings = FALSE) %>>% po(lrn)
  expect_graph(p)
  expect_true(all(c("char_to_fct", "fixfactors", "collapsefactors", "encode") %in% names(p$pipeops)))
  expect_true(all(c("imputehist", "missind", "imputeoor") %nin% names(p$pipeops)))

  # missings during predict
  dt = tsk$data()
  dt[2, 3] = NA
  tsk2 = TaskRegr$new(id = "bh", dt, target = "medv")
  lrn$properties = c("multiclass", "twoclass")
  p = ppl("robustify", impute_missings = TRUE) %>>% po(lrn)
  g = GraphLearner$new(p)
  g$train(tsk)
  prd = g$predict(tsk2)
  expect_prediction(prd)

  # date features
  dat = iris
  set.seed(1)
  dat$date = sample(seq(as.POSIXct("2020-02-01"), to = as.POSIXct("2020-02-29"), by = "hour"),
   size = 150L)
  tsk = TaskClassif$new("iris_date", backend = dat, target = "Species")
  p = pipeline_robustify(task = tsk, learner = lrn) %>>% po(lrn)
  expect_graph(p)
  expect_true("removeconstants_postrobustify" %in% names(p$pipeops))
  expect_true("POSIXct_to_dbl" %in% names(p$pipeops))
  expect_true(length(p$pipeops) == 4)

  p = pipeline_robustify(task = tsk, learner = lrn, POSIXct_action = "datefeatures") %>>% po(lrn)
  expect_graph(p)
  expect_true("removeconstants_postrobustify" %in% names(p$pipeops))
  expect_true("datefeatures" %in% names(p$pipeops))
  expect_true(length(p$pipeops) == 4)

})



test_that("Robustify Pipeline Impute Missings", {
  tmissings = tsk("pima")
  tnomissings = tsk("iris")

  lmissings = lrn("classif.rpart")
  lnomissings = lrn("classif.rpart")
  lnomissings$properties = setdiff(lnomissings$properties, "missings")

  micols = tmissings$missings()[tmissings$missings() != 0]
  names(micols) = paste0("missing_", names(micols))
  sort_names = function(x) x[order(names(x))]

  expect_equal(ppl("robustify", learner = lmissings, task = tmissings)$train(tmissings)[[1]]$missings(), tmissings$missings())
  expect_equal(
    sort_names(ppl("robustify", learner = lnomissings, task = tmissings)$train(tmissings)[[1]]$missings()),
    sort_names(c(tmissings$missings(), micols) * 0)
  )

  expect_equal(
    sort_names(ppl("robustify", learner = lmissings, task = tmissings, impute_missings = TRUE)$train(tmissings)[[1]]$missings()),
    sort_names(c(tmissings$missings(), micols) * 0)
  )

  expect_equal(
    sort_names(ppl("robustify", learner = lnomissings, task = tnomissings, impute_missings = TRUE)$train(tmissings)[[1]]$missings()),
    sort_names(c(tmissings$missings(), micols) * 0)
  )

  expect_equal(ppl("robustify", learner = lnomissings, task = tnomissings)$train(tmissings)[[1]]$missings(), tmissings$missings())

})

makeTypeTask = function(types) {
  letters5 = paste0(letters, letters, letters, letters, letters)
  cols = c(
    if ("integer" %in% types) list(i1 = 1:10, i2 = 10:1),
    if ("numeric" %in% types) list(n1 = seq(0, 1, length.out = 10), n2 = seq(1, 0, length.out = 10)),
    if ("logical" %in% types) list(l1 = rep(c(TRUE, FALSE), 5), l2 = rep(c(FALSE, TRUE), 5)),
    if ("character" %in% types) list(c1 = paste(letters5[1:10], letters5[10:1]), c2 = paste(letters5[11:20], letters5[20:11])),
    if ("POSIXct" %in% types) list(
      p1 = seq(as.POSIXct("2020-02-01"), to = as.POSIXct("2020-02-29"), length.out = 10),
      p2 = seq(as.POSIXct("2020-02-29"), to = as.POSIXct("2020-02-01"), length.out = 10)
    )
  )
  TaskRegr$new("typetask", as.data.table(c(cols, list(target = seq(-1, 1, length.out = 10)))), target = "target")
}


test_that("Robustify Pipeline factor to numeric", {

  alltask = makeTypeTask(c("integer", "numeric", "logical", "character", "POSIXct"))

  skip_if_not_installed("quanteda")
  suppressWarnings(loadNamespace("quanteda"))  # TODO: see https://github.com/quanteda/quanteda/issues/2116 , may not be an issue in the future

  lfactor = lrn("regr.rpart")
  lnofactor = lrn("regr.rpart")
  lnofactor$feature_types = setdiff(lnofactor$feature_types, "factor")

  atft = alltask$feature_types
  cleanedatft = copy(atft)[type == "character", type := "factor"][type == "POSIXct", type := "numeric"]
  vectoradft = copy(po("textvectorizer")$train(list(alltask))[[1]]$feature_types)[type == "POSIXct", type := "numeric"]

  expect_equal(ppl("robustify", learner = lfactor, makeTypeTask("numeric"))$train(alltask)[[1]]$feature_types, atft, check.attributes = FALSE)
  expect_equal(ppl("robustify", learner = lnofactor, makeTypeTask("numeric"))$train(alltask)[[1]]$feature_types, atft, check.attributes = FALSE)
  expect_equal(ppl("robustify", learner = lfactor, alltask)$train(alltask)[[1]]$feature_types, cleanedatft, ignore.row.order = TRUE, check.attributes = FALSE)


  expect_equal(ppl("robustify", learner = lnofactor, alltask)$train(alltask)[[1]]$feature_types[, id := gsub("\\.[^.]*$", "", id)], vectoradft, check.attributes = FALSE)
  expect_equal(ppl("robustify", learner = lnofactor, alltask, character_action = "matrix")$train(alltask)[[1]]$feature_types, vectoradft, check.attributes = FALSE)


})
