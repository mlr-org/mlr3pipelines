context("conversion")

test_that("type conversions in graph creation", {
  gr1 = Graph$new()$add_pipeop(po("scale"))
  gr2 = Graph$new()$add_pipeop(PipeOpScale$new())
  gr3 = as_graph(PipeOpScale$new())
  gr4 = Graph$new()$add_pipeop(mlr_pipeops$get("scale"))

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)
  expect_equal(gr1, gr4)

  gr1 = po("pca") %>>% PipeOpScale$new()
  gr2 = PipeOpPCA$new() %>>% PipeOpScale$new()
  gr3 = po("pca") %>>% gr3

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)

  expect_equal(gr1, gunion(list(po("pca"), po("scale")))$add_edge("pca", "scale"))

})

test_that("learner conversion in graph creation", {
  gr1 = Graph$new()$add_pipeop(lrn("classif.rpart"))
  gr2 = Graph$new()$add_pipeop(LearnerClassifRpart$new())
  gr3 = Graph$new()$add_pipeop(mlr_pipeops$get("learner", lrn("classif.rpart")))
  gr4 = Graph$new()$add_pipeop(PipeOpLearner$new(mlr_learners$get("classif.rpart")))

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)
  expect_equal(gr1, gr4)

  gr1 = po("scale") %>>% lrn("classif.rpart")
  gr2 = po("scale") %>>% mlr_pipeops$get("learner", lrn("classif.rpart"))
  gr3 = po("scale") %>>% LearnerClassifRpart$new()
  gr4 = po("scale") %>>% PipeOpLearner$new(mlr_learners$get("classif.rpart"))

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)
  expect_equal(gr1, gr4)

  expect_equal(mlr_pipeops$get("learner_cv", lrn("classif.rpart")),
    PipeOpLearnerCV$new(mlr_learners$get("classif.rpart")))
})

test_that("assertions work", {

  expect_error(as_pipeop("test"))
  expect_error(assert_pipeop(lrn("classif.rpart")))
  expect_class(as_pipeop(lrn("classif.rpart")), "PipeOp")

  expect_error(as_graph("test"))
  expect_class(as_graph(po("scale")), "Graph")

  expect_class(as_pipeop(lrn("classif.rpart")), "PipeOp")
  expect_class(as_graph(lrn("classif.rpart")), "Graph")

  expect_error(as_pipeop(Graph))
  expect_class(as_pipeop(PipeOpScale$new()), "PipeOp")

  expect_error(as_graph(Graph))
  expect_class(as_graph(PipeOpScale$new()), "Graph")

  # proximity matching
  expect_error(po("scule") %>>% po("pca"), "scale")
  expect_error(po("scale") %>>% lrn("classif.rpurt"), "classif.rpart")
})


test_that("auto-gunion", {

  expect_equal(
    list(po("pca"), po("scale")) %>>% list(po("subsample"), po("nop")),
    gunion(list(mlr_pipeops$get("pca"), mlr_pipeops$get("scale"))) %>>%
      gunion(list(mlr_pipeops$get("subsample"), mlr_pipeops$get("nop")))
  )

  expect_equal(
    list(po("pca"), po("scale")) %>>% mlr_pipeops$get("featureunion", 2),
    gunion(list(mlr_pipeops$get("pca"), mlr_pipeops$get("scale"))) %>>%
      PipeOpFeatureUnion$new(2)
  )

})

test_that("po for Filter", {
  flt = mlr3filters::FilterVariance$new()
  flt$param_set$values$na.rm = TRUE

  fpo1 = PipeOpFilter$new(flt, param_vals = list(na.rm = FALSE))

  fpo2 = po("filter", flt, na.rm = FALSE)

  fpo3 = po(flt, na.rm = FALSE)

  expect_equal(fpo1, fpo2)
  expect_equal(fpo1, fpo3)

})

test_that("po for Learner", {
  lrn = LearnerClassifRpart$new()
  lrn$param_set$values$xval = 9

  lpo1 = PipeOpLearner$new(lrn, param_vals = list(xval = 1))

  lpo2 = po("learner", lrn, xval = 1)

  lpo3 = po(lrn, xval = 1)

  expect_equal(lpo1, lpo2)
  expect_equal(lpo1, lpo3)

})
