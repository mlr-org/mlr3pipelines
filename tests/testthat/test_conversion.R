context("conversion")

test_that("type conversions in graph creation", {
  gr1 = Graph$new()$add_pipeop("scale")
  gr2 = Graph$new()$add_pipeop(PipeOpScale)
  gr3 = Graph$new()$add_pipeop(PipeOpScale$new())
  gr4 = Graph$new()$add_pipeop(po("scale"))

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)
  expect_equal(gr1, gr4)

  gr1 = "pca" %>>% PipeOpScale
  gr2 = PipeOpPCA %>>% PipeOpScale$new()
  gr3 = "pca" %>>% gr3

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)

  expect_equal(gr1, gunion(list("pca", "scale"))$add_edge("pca", "scale"))

})

test_that("learner conversion in graph creation", {
  gr1 = Graph$new()$add_pipeop("classif.rpart")
  gr2 = Graph$new()$add_pipeop(LearnerClassifRpart)
  gr3 = Graph$new()$add_pipeop(po("learner", "classif.rpart"))
  gr4 = Graph$new()$add_pipeop(PipeOpLearner$new(mlr_learners$get("classif.rpart")))

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)
  expect_equal(gr1, gr4)

  gr1 = "scale" %>>% "classif.rpart"
  gr2 = "scale" %>>% po("learner", "classif.rpart")
  gr3 = "scale" %>>% LearnerClassifRpart
  gr4 = "scale" %>>% PipeOpLearner$new(mlr_learners$get("classif.rpart"))

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)
  expect_equal(gr1, gr4)

  expect_equal(po("learner_cv", "classif.rpart"),
    PipeOpLearnerCV$new(mlr_learners$get("classif.rpart")))

})

test_that("assertions work", {

  expect_error(assert_pipeop("test", coerce = TRUE))
  expect_class(assert_pipeop("scale", coerce = TRUE), "PipeOp")

  expect_error(assert_graph("test", coerce = TRUE))
  expect_class(assert_graph("scale", coerce = TRUE), "Graph")

  expect_class(assert_pipeop("classif.rpart", coerce = TRUE), "PipeOp")
  expect_class(assert_graph("classif.rpart", coerce = TRUE), "Graph")

  expect_error(assert_pipeop(Graph, coerce = TRUE))
  expect_class(assert_pipeop(PipeOpScale, coerce = TRUE), "PipeOp")

  expect_error(assert_graph(Graph, coerce = TRUE))
  expect_class(assert_graph(PipeOpScale, coerce = TRUE), "Graph")

  # proximity matching
  expect_error("scule" %>>% "pca", "scale")
  expect_error("scale" %>>% "classif.rpurt", "classif.rpart")
})


test_that("auto-gunion", {

  expect_equal(
    list("pca", "scale") %>>% list("subsample", "null"),
    gunion(list(mlr_pipeops$get("pca"), mlr_pipeops$get("scale"))) %>>%
      gunion(list(mlr_pipeops$get("subsample"), mlr_pipeops$get("null")))
  )

  expect_equal(
    list("pca", "scale") %>>% po("featureunion", 2),
    gunion(list(mlr_pipeops$get("pca"), mlr_pipeops$get("scale"))) %>>%
      PipeOpFeatureUnion$new(2)
  )

})
