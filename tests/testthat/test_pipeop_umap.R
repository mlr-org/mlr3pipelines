context("PipeOpUMAP")

test_that("PipeOpUMAP - basic properties", {
  skip_if_not_installed("uwot")
  op = PipeOpUMAP$new()
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(op, task = task)
})

test_that("PipeOpUMAP - Compare to uwot::umap2", {
  skip_if_not_installed("uwot")
  op = PipeOpUMAP$new()
  task = mlr_tasks$get("iris")

  # Default parameters

  # Some changed parameters

})
