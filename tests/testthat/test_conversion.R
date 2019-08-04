context("conversion")

test_that("type conversions in graph creation", {
  gr1 = Graph$new()$add_pipeop("scale")
  gr2 = Graph$new()$add_pipeop(PipeOpScale$new())
  gr3 = Graph$new()$add_pipeop(PipeOpScale$new())
  gr4 = Graph$new()$add_pipeop(mlr_pipeops$get("scale"))

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)
  expect_equal(gr1, gr4)

  gr1 = "pca" %>>% PipeOpScale$new()
  gr2 = PipeOpPCA$new() %>>% PipeOpScale$new()
  gr3 = "pca" %>>% gr3

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)

  expect_equal(gr1, gunion(list("pca", "scale"))$add_edge("pca", "scale"))

})

test_that("learner conversion in graph creation", {
  gr1 = Graph$new()$add_pipeop("classif.rpart")
  gr2 = Graph$new()$add_pipeop(LearnerClassifRpart$new())
  gr3 = Graph$new()$add_pipeop(mlr_pipeops$get("learner", "classif.rpart"))
  gr4 = Graph$new()$add_pipeop(PipeOpLearner$new(mlr_learners$get("classif.rpart")))

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)
  expect_equal(gr1, gr4)

  gr1 = "scale" %>>% "classif.rpart"
  gr2 = "scale" %>>% mlr_pipeops$get("learner", "classif.rpart")
  gr3 = "scale" %>>% LearnerClassifRpart$new()
  gr4 = "scale" %>>% PipeOpLearner$new(mlr_learners$get("classif.rpart"))

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)
  expect_equal(gr1, gr4)

  expect_equal(mlr_pipeops$get("learner_cv", "classif.rpart"),
    PipeOpLearnerCV$new(mlr_learners$get("classif.rpart")))

})

test_that("assertions work", {

  expect_error(as_pipeop("test"))
  expect_error(assert_pipeop("scale"))
  expect_class(as_pipeop("scale"), "PipeOp")

  expect_error(as_graph("test"))
  expect_class(as_graph("scale"), "Graph")

  expect_class(as_pipeop("classif.rpart"), "PipeOp")
  expect_class(as_graph("classif.rpart"), "Graph")

  expect_error(as_pipeop(Graph))
  expect_class(as_pipeop(PipeOpScale$new()), "PipeOp")

  expect_error(as_graph(Graph))
  expect_class(as_graph(PipeOpScale$new()), "Graph")

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
    list("pca", "scale") %>>% mlr_pipeops$get("featureunion", 2),
    gunion(list(mlr_pipeops$get("pca"), mlr_pipeops$get("scale"))) %>>%
      PipeOpFeatureUnion$new(2)
  )

})
