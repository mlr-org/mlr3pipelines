context("GraphParamSet")

test_that("graph param vals", {
  gr = Graph$new()
  gr$add_pipeop(PipeOpScale$new())
  gr$add_pipeop(PipeOpPCA$new())
  expect_equal(gr$ids(TRUE), c("scale", "pca"))

  expect_null(gr$pipeops$scale$param_set$values$center)
  expect_null(gr$param_set$values$scale.center)
  gr$param_set$values$scale.center = FALSE
  expect_equal(gr$pipeops$scale$param_set$values$center, FALSE)
  expect_equal(gr$param_set$values$scale.center, FALSE)
  gr$pipeops$scale$param_set$values$center = TRUE
  expect_equal(gr$pipeops$scale$param_set$values$center, TRUE)
  expect_equal(gr$param_set$values$scale.center, TRUE)


  expect_null(gr$pipeops$pca$param_set$values$center)
  expect_null(gr$param_set$values$pca.center)
  gr$param_set$values$pca.center = FALSE
  expect_equal(gr$pipeops$pca$param_set$values$center, FALSE)
  expect_equal(gr$param_set$values$pca.center, FALSE)
  gr$pipeops$pca$param_set$values$center = TRUE
  expect_equal(gr$pipeops$pca$param_set$values$center, TRUE)
  expect_equal(gr$param_set$values$pca.center, TRUE)

  expect_set_equal(gr$param_set$ids(),
    c("scale.center", "scale.scale" ,"scale.robust", "scale.affect_columns", "pca.center", "pca.scale.", "pca.rank.", "pca.affect_columns"))

  expect_error({
    gr$pipeops$pca$param_set$values$center = 1
  })  # type mismatch
  expect_error({
    gr$param_set$values$pca.center = 1
  })
})

test_that("graph has value changes when param vals change", {
  gr = Graph$new()
  gr$add_pipeop(PipeOpScale$new())
  gr$add_pipeop(PipeOpPCA$new())
  expect_equal(gr$ids(TRUE), c("scale", "pca"))


  gr$param_set$values$pca.center = FALSE
  hashbeginning = gr$hash
  gr$param_set$values$pca.center = TRUE
  hashend = gr$hash
  expect_character(hashbeginning, len = 1)
  expect_character(hashend, len = 1)
  expect_true(hashbeginning != hashend)
})
