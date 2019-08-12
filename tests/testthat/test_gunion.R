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

test_that("named gunion", {
  expect_equal(
    gunion(list(a = "scale", b = "pca", "subsample")),
    Graph$new()$
      add_pipeop(PipeOpScale$new(id = "a.scale"))$
      add_pipeop(PipeOpPCA$new(id = "b.pca"))$
      add_pipeop(PipeOpSubsample$new())
  )

  expect_equal(
    gunion(list(
      a = "scale",
      b = "subsample" %>>% "pca",
      "subsample",
      z = gunion(list("pca", "scale")) %>>% "featureunion")) %>>%
      PipeOpFeatureUnion$new(4),
    Graph$new()$
      add_pipeop(PipeOpScale$new(id = "a.scale"))$
      add_pipeop(PipeOpSubsample$new(id = "b.subsample"))$
      add_pipeop(PipeOpPCA$new(id = "b.pca"))$
      add_edge("b.subsample", "b.pca")$
      add_pipeop(PipeOpSubsample$new())$
      add_pipeop(PipeOpPCA$new(id = "z.pca"))$
      add_pipeop(PipeOpScale$new(id = "z.scale"))$
      add_pipeop(PipeOpFeatureUnion$new(id = "z.featureunion"))$
      add_edge("z.pca", "z.featureunion")$
      add_edge("z.scale", "z.featureunion")$
      add_pipeop(PipeOpFeatureUnion$new(innum = 4))$
      add_edge("a.scale", "featureunion", dst_channel = "input1")$
      add_edge("b.pca", "featureunion", dst_channel = "input2")$
      add_edge("subsample", "featureunion", dst_channel = "input3")$
      add_edge("z.featureunion", "featureunion", dst_channel = "input4")
  )
})
