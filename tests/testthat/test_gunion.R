context("gunion")

test_that("gunion", {
  g1 = PipeOpPCA$new() %>>% PipeOpScale$new(param_vals = list(scale = FALSE))
  g2 = PipeOpPCA$new(id = "pca2") %>>% PipeOpPCA$new(id = "xx")
  g3 = PipeOpScale$new(id = "blub") %>>% PipeOpScale$new(id = "foo")
  g4 = gunion(list(g1, g2, g3))
  expect_graph(g4)
  expect_set_equal(g4$ids(), c("pca", "pca2", "scale", "xx", "blub", "foo"))

  expect_deep_clone(g1, PipeOpPCA$new() %>>% PipeOpScale$new(param_vals = list(scale = FALSE)))

  params = g4$param_set$params

  g4_2 = gunion(list(g1, g2, g3), in_place = TRUE)

  expect_identical(g4_2, g1)
  expect_deep_clone(touch(g4_2), touch(g4))

  # make sure param_set was properly built.
  expect_deep_clone(params, g4_2$param_set$params)


  g0 = PipeOpNOP$new()
  g4 = gunion(list(g0, g2, g3))

  expect_set_equal(g4$ids(), c("nop", "pca2", "xx", "blub", "foo"))

  expect_deep_clone(g0, PipeOpNOP$new())

  g4_2 = gunion(list(g0, g2, g3), in_place = TRUE)

  expect_deep_clone(g0, PipeOpNOP$new())
  expect_deep_clone(touch(g4_2), touch(g4))






})

test_that("empty gunion", {
  expect_equal(gunion(list()), Graph$new())
  expect_equal(gunion(list(), in_place = TRUE), Graph$new())
})

test_that("named gunion", {

  expect_equal(
    touch(gunion(list(a = po("scale"), b = po("pca"), po("subsample")))),
    touch(Graph$new()$
      add_pipeop(PipeOpScale$new(id = "a.scale"))$
      add_pipeop(PipeOpPCA$new(id = "b.pca"))$
      add_pipeop(PipeOpSubsample$new()))
  )

  expect_equal(
    touch(gunion(list(
      a = po("scale"),
      b = po("subsample") %>>% po("pca"),
      po("subsample"),
      z = gunion(list(po("pca"), po("scale"))) %>>% po("featureunion"))) %>>%
      PipeOpFeatureUnion$new(4)),
    touch(Graph$new()$
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
      add_edge("z.featureunion", "featureunion", dst_channel = "input4"))
  )

})
