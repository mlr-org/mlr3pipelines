context("greplicate")

test_that("greplicate / pipeop", {
  op_ds = PipeOpDownsample$new()
  g = greplicate(op_ds, 3)
  expect_graph(g)
  expect_character(g$ids(), len = 3)
})
