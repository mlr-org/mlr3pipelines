context("Graph")

test_that("graph param vals", {

  gr = Graph$new()
  gr$add_pipeop(PipeOpScale$new())
  gr$add_pipeop(PipeOpPCA$new())
  expect_equal(gr$ids(TRUE), c("scale", "pca"))

  expect_equal(gr$pipeops$scale$param_vals$center, TRUE)
  expect_equal(gr$param_vals$scale.center, TRUE)
  gr$param_vals$scale.center = FALSE
  expect_equal(gr$pipeops$scale$param_vals$center, FALSE)
  expect_equal(gr$param_vals$scale.center, FALSE)
  gr$pipeops$scale$param_vals$center = TRUE
  expect_equal(gr$pipeops$scale$param_vals$center, TRUE)
  expect_equal(gr$param_vals$scale.center, TRUE)


  expect_equal(gr$pipeops$pca$param_vals$center, TRUE)
  expect_equal(gr$param_vals$pca.center, TRUE)
  gr$param_vals$pca.center = FALSE
  expect_equal(gr$pipeops$pca$param_vals$center, FALSE)
  expect_equal(gr$param_vals$pca.center, FALSE)
  gr$pipeops$pca$param_vals$center = TRUE
  expect_equal(gr$pipeops$pca$param_vals$center, TRUE)
  expect_equal(gr$param_vals$pca.center, TRUE)

  expect_set_equal(names(gr$param_set$params),
    c("scale.center", "scale.scale", "pca.center", "pca.scale.", "pca.rank."))

  expect_error({gr$pipeops$pca$param_vals$center = 1})  # type mismatch
  expect_error({gr$param_vals$pca.center = 1})

})

