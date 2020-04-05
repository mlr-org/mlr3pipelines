context("ppl - pipeline_greplicate")

test_that("Greplicate Pipeline", {
  op_ds = PipeOpSubsample$new()
  g = pipeline_greplicate(op_ds, n = 3L)
  expect_graph(g)
  expect_character(g$ids(), len = 3L)
  expect_equal(names(g$pipeops), c("subsample_1", "subsample_2", "subsample_3"))
})
