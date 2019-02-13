context("greplicate")

test_that("greplicate / pipeop", {
  op_ds = PipeOpSubsample$new()
  g = greplicate(op_ds, 3)
  expect_graph(g)
  expect_character(g$ids(), len = 3)
  expect_equal(names(g$pipeops), c("subsample_1", "subsample_2", "subsample_3"))
})
