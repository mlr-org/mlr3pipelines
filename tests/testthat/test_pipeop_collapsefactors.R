context("PipeOpCollapseFactors")

test_that("PipeOpCollapseFactors - basic properties", {
  task = mlr_tasks$get("penguins")

  expect_datapreproc_pipeop_class(PipeOpCollapseFactors, task = task)
})

test_that("PipeOpCollapseFactors", {
  op = PipeOpCollapseFactors$new()
  df = data.frame(
    target = runif(100),
    fct = factor(rep(LETTERS[1:6], times = c(25, 30, 5, 15, 5, 20))),
    ord = factor(rep(1:6, times = c(20, 25, 30, 4, 6, 15)), ordered = TRUE)
  )
  task = TaskRegr$new(df, target = "target", id = "test")

  # test (default): levels are reduced to target_count
  #                 correct levels are chosen for this
  train_out = op$train(list(task))[[1]]

  # test: ordered, correct renaming

  # test: prevalance works
  # test: absolute works
  # test: if given both, does as documented (i.e. higher one is used since we are using union)
  # test: interaction between target_count and prev/abs; also think about whether current behavior makes sense; document it
})
