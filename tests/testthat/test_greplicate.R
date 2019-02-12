context("greplicate")

test_that("greplicate / pipeop", {
  op_ds = PipeOpSubsample$new()
  g = greplicate(op_ds, 3)
  expect_graph(g)
  expect_character(g$ids(), len = 3)
  expect_equal(names(g$pipeops), c("downsample_1", "downsample_2", "downsample_3"))
})
