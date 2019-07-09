context("conversion")

test_that("type conversions in graph creation", {
  gr1 = Graph$new()$add_pipeop("scale")
  gr2 = Graph$new()$add_pipeop(PipeOpScale)
  gr3 = Graph$new()$add_pipeop(PipeOpScale$new())

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)

  gr1 = "pca" %>>% PipeOpScale
  gr2 = PipeOpPCA %>>% PipeOpScale$new()
  gr3 = "pca" %>>% gr3

  expect_equal(gr1, gr2)
  expect_equal(gr1, gr3)

  expect_equal(gr1, gunion(list("pca", "scale"))$add_edge("pca", "scale"))

})

test_that("assertions work", {

  expect_error(assert_pipeop("test", coerce = TRUE))
  expect_class(assert_pipeop("scale", coerce = TRUE), "PipeOp")

  expect_error(assert_graph("test", coerce = TRUE))
  expect_class(assert_graph("scale", coerce = TRUE), "Graph")

  expect_error(assert_pipeop(Graph, coerce = TRUE))
  expect_class(assert_pipeop(PipeOpScale, coerce = TRUE), "PipeOp")

  expect_error(assert_graph(Graph, coerce = TRUE))
  expect_class(assert_graph(PipeOpScale, coerce = TRUE), "Graph")

})
