context("PipeOpProxy")

test_that("PipeOpProxy - basic properties", {
  pop = PipeOpProxy$new(param_vals = list(content = PipeOpNOP$new()))
  task = mlr_tasks$get("iris")
  expect_pipeop(pop)
  train_pipeop(pop, inputs = list(task))
  expect_pipeop(pop$state)
  predict_pipeop(pop, inputs = list(task))
  expect_error(PipeOpProxy$new(param_vals = list(content = "error")))
})

test_that("PipeOpProxy - PCA proxied", {
  pop = PipeOpProxy$new(param_vals = list(content =
    PipeOpPCA$new(param_vals = list(center = TRUE, scale. = TRUE, rank. = 1L))))
  pop_pca = PipeOpPCA$new(param_vals = list(center = TRUE, scale. = TRUE, rank. = 1L))
  task1 = mlr_tasks$get("iris")
  task2 = task1$clone(deep = TRUE)
  tout = train_pipeop(pop, list(task1))
  tout_pca = train_pipeop(pop_pca, list(task2))
  expect_equal(tout[[1L]], tout_pca[[1L]])
})

test_that("PipeOpProxy - Graph proxied", {
  pop = PipeOpProxy$new(param_vals = list(content =
    PipeOpScale$new() %>>%
    PipeOpPCA$new(param_vals = list(center = TRUE, scale. = TRUE, rank. = 1L))))
  gr = PipeOpScale$new() %>>%
    PipeOpPCA$new(param_vals = list(center = TRUE, scale. = TRUE, rank. = 1L))
  task1 = mlr_tasks$get("iris")
  task2 = task1$clone(deep = TRUE)
  tout = train_pipeop(pop, list(task1))
  tout_gr = gr$train(task2)
  expect_equal(tout[[1L]], tout_gr[[1L]])
})
