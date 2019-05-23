context("gunion")

test_that("gunion", {
  g1 = PipeOpPCA$new() %>>% PipeOpScale$new()
  g2 = PipeOpPCA$new(id = "pca2") %>>% PipeOpPCA$new(id = "xx")
  g3 = PipeOpScale$new(id = "blub") %>>% PipeOpScale$new(id = "foo")
  g4 = gunion(list(g1, g2, g3))
  expect_graph(g4)
  expect_set_equal(g4$ids(), c("pca", "pca2", "scale", "xx", "blub", "foo"))
})

test_that("empty gunion", {
  expect_equal(gunion(list()), Graph$new())
})
